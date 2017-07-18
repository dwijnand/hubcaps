//! Git interface

extern crate serde_json;
use self::super::{Github, Result};

/// reference to git operations associated with a github repo
pub struct Git<'a> {
    github: &'a Github,
    owner: String,
    repo: String,
}

impl<'a> Git<'a> {
    #[doc(hidden)]
    pub fn new<O, R>(github: &'a Github, owner: O, repo: R) -> Self
    where
        O: Into<String>,
        R: Into<String>,
    {
        Git {
            github: github,
            owner: owner.into(),
            repo: repo.into(),
        }
    }

    fn path(&self, more: String) -> String {
        format!("/repos/{}/{}/git{}", self.owner, self.repo, more)
    }

    /// list a git tree of files for this repo at a given sha
    /// https://developer.github.com/v3/git/trees/#get-a-tree
    /// https://developer.github.com/v3/git/trees/#get-a-tree-recursively
    pub fn tree<S>(&self, sha: S, recursive: bool) -> Result<TreeData>
    where
        S: Into<String>,
    {
        self.github.get::<TreeData>(&self.path(format!(
            "/trees/{}?recursive={}",
            sha.into(),
            if recursive { "1" } else { "0" }
        )))
    }

    /// get the blob contents of a given sha
    /// https://developer.github.com/v3/git/blobs/#get-a-blob
    pub fn blob<S>(&self, sha: S) -> Result<Blob>
    where
        S: Into<String>,
    {
        self.github.get::<Blob>(
            &self.path(format!("/blobs/{}", sha.into())),
        )
    }

    /// get the git reference data of a given ref
    /// the specified reference must be formatted as as "heads/branch", not just "branch"
    /// https://developer.github.com/v3/git/refs/#get-a-reference
    pub fn reference<S>(&self, reference: S) -> Result<GetReferenceResponse>
    where
        S: Into<String>,
    {
        self.github.get::<GetReferenceResponse>(
            &self.path(format!("/refs/{}", reference.into())),
        )
    }

    /// Provides access to git commit operations associated with a github repo
    pub fn commits(&self) -> Commits {
        Commits::new(self.github, self.owner.as_str(), self.repo.as_str())
    }
}

/// Provides access to git commit operations associated with a github repo
/// Typically accessed via `github.repo(..., ...).git().commits()`
pub struct Commits<'a> {
    github: &'a Github,
    owner: String,
    repo: String,
}

impl<'a> Commits<'a> {
    #[doc(hidden)]
    pub fn new<O, R>(github: &'a Github, owner: O, repo: R) -> Self
    where
        O: Into<String>,
        R: Into<String>,
    {
        Commits {
            github: github,
            owner: owner.into(),
            repo: repo.into(),
        }
    }

    /// Create a commit: https://developer.github.com/v3/git/commits/#create-a-commit
    pub fn create(&self, commit: &CommitOptions) -> Result<Commit> {
        let data = serde_json::to_string(&commit)?;
        self.github.post::<Commit>(&self.path(""), data.as_bytes())
    }

    fn path(&self, more: &str) -> String {
        format!("/repos/{}/{}/git/commits{}", self.owner, self.repo, more)
    }
}


// representations

#[derive(Debug, Deserialize)]
pub struct TreeData {
    pub sha: String,
    pub url: String,
    pub tree: Vec<GitFile>,
    pub truncated: bool,
}

#[derive(Debug, Deserialize)]
pub struct GitFile {
    pub path: String,
    pub mode: String,
    /// typically tree or blob
    #[serde(rename = "type")]
    pub content_type: String,
    /// size will be None for directories
    pub size: Option<usize>,
    pub sha: String,
    /// url will be None for commits
    pub url: Option<String>,
}

#[derive(Debug, Deserialize)]
pub struct Blob {
    pub content: String,
    pub encoding: String,
    pub url: String,
    pub sha: String,
    /// sizes will be None for directories
    pub size: Option<usize>,
}

#[derive(Debug, Deserialize, PartialEq)]
#[serde(untagged)]
/// The response for getting a git reference
pub enum GetReferenceResponse {
    /// The reference data matching the specified reference
    Exact(Reference),
    /// If the reference doesn't exist in the repository
    /// but existing refs start with ref they will be returned as an array.
    /// For example, a call to get the data for a branch named feature,
    /// which doesn't exist, would return head refs including featureA and featureB which do.
    StartWith(Vec<Reference>),
}

#[derive(Debug, Deserialize, PartialEq)]
pub struct Reference {
    #[serde(rename = "ref")]
    pub reference: String,
    pub url: String,
    pub object: Object,
}

#[derive(Debug, Deserialize, PartialEq)]
pub struct Object {
    #[serde(rename = "type")]
    pub object_type: String,
    pub sha: String,
    pub url: String,
}

#[derive(Debug, Deserialize)]
pub struct Commit {
    pub sha: String,
    pub url: String,
    pub author: Signature,
    pub committer: Signature,
    pub message: String,
    pub tree: ObjectRef,
    pub parents: Vec<ObjectRef>,
}

#[derive(Debug, Deserialize, Clone, PartialEq, Serialize)]
pub struct Signature {
    pub name: String,
    pub email: String,
    pub date: String,
}

impl Signature {
    pub fn new<N, E, D>(name: N, email: E, date: D) -> Self
    where
        N: Into<String>,
        E: Into<String>,
        D: Into<String>,
    {
        Signature {
            name: name.into(),
            email: email.into(),
            date: date.into(),
        }
    }
}

#[derive(Debug, Deserialize)]
pub struct ObjectRef {
    pub url: String,
    pub sha: String,
}

#[derive(Debug, Default, PartialEq, Serialize)]
pub struct CommitOptions {
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub author: Option<Signature>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub committer: Option<Signature>,
    pub parents: Vec<String>,
    pub tree: String,
}

impl CommitOptions {
    pub fn builder<M, T, P>(message: M, tree: T, parents: Vec<P>) -> CommitOptionsBuilder
    where
        M: Into<String>,
        T: Into<String>,
        P: Into<String>,
    {
        CommitOptionsBuilder::new(message, tree, parents)
    }
}

pub struct CommitOptionsBuilder(CommitOptions);

impl CommitOptionsBuilder {
    #[doc(hidden)]
    pub fn new<M, T, P>(message: M, tree: T, parents: Vec<P>) -> CommitOptionsBuilder
    where
        M: Into<String>,
        T: Into<String>,
        P: Into<String>,
    {
        CommitOptionsBuilder(CommitOptions {
            message: message.into(),
            tree: tree.into(),
            parents: parents
                .into_iter()
                .map(|c| c.into())
                .collect::<Vec<String>>(),
            ..Default::default()
        })
    }

    pub fn committer<N, E, D>(&mut self, name: N, email: E, date: D) -> &mut Self
    where
        N: Into<String>,
        E: Into<String>,
        D: Into<String>,
    {
        self.0.committer = Some(Signature::new(name, email, date));
        self
    }

    pub fn author<N, E, D>(&mut self, name: N, email: E, date: D) -> &mut Self
    where
        N: Into<String>,
        E: Into<String>,
        D: Into<String>,
    {
        self.0.author = Some(Signature::new(name, email, date));
        self
    }

    pub fn build(&self) -> CommitOptions {
        CommitOptions {
            message: self.0.message.clone(),
            tree: self.0.tree.clone(),
            parents: self.0.parents.clone(),
            committer: self.0.committer.clone(),
            author: self.0.author.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;
    use serde::{Deserialize, Serialize};
    use super::*;

    fn test_deserializing<'de, T>(payload: &'static str, expected: T)
    where
        T: Debug + PartialEq + Deserialize<'de>,
    {
        let incoming: T = serde_json::from_str(payload).unwrap();
        assert_eq!(incoming, expected)
    }

    fn test_serializing<T>(original: T, expected: &'static str)
    where
        T: PartialEq + Serialize,
    {
        let incoming = serde_json::to_string(&original).unwrap();
        assert_eq!(incoming, expected)
    }

    #[test]
    fn deserialize_get_ref_exact() {
        let payload = r#"{
  "ref": "refs/heads/featureA",
  "url": "https://api.github.com/repos/octocat/Hello-World/git/refs/heads/featureA",
  "object": {
    "type": "commit",
    "sha": "aa218f56b14c9653891f9e74264a383fa43fefbd",
    "url": "https://api.github.com/repos/octocat/Hello-World/git/commits/aa218f56b14c9653891f9e74264a383fa43fefbd"
  }
}"#;
        let expected = GetReferenceResponse::Exact(Reference {
            reference: "refs/heads/featureA".to_string(),
            url: "https://api.github.com/repos/octocat/Hello-World/git/refs/heads/featureA".to_string(),
            object: Object {
                object_type: "commit".to_string(),
                sha: "aa218f56b14c9653891f9e74264a383fa43fefbd".to_string(),
                url: "https://api.github.com/repos/octocat/Hello-World/git/commits/aa218f56b14c9653891f9e74264a383fa43fefbd".to_string(),
            },
        });
        test_deserializing(payload, expected)
    }

    #[test]
    fn deserialize_get_ref_starts_with() {
        let payload = r#"[
  {
    "ref": "refs/heads/feature-a",
    "url": "https://api.github.com/repos/octocat/Hello-World/git/refs/heads/feature-a",
    "object": {
      "type": "commit",
      "sha": "aa218f56b14c9653891f9e74264a383fa43fefbd",
      "url": "https://api.github.com/repos/octocat/Hello-World/git/commits/aa218f56b14c9653891f9e74264a383fa43fefbd"
    }
  },
  {
    "ref": "refs/heads/feature-b",
    "url": "https://api.github.com/repos/octocat/Hello-World/git/refs/heads/feature-b",
    "object": {
      "type": "commit",
      "sha": "612077ae6dffb4d2fbd8ce0cccaa58893b07b5ac",
      "url": "https://api.github.com/repos/octocat/Hello-World/git/commits/612077ae6dffb4d2fbd8ce0cccaa58893b07b5ac"
    }
  }
]"#;
        let expected = GetReferenceResponse::StartWith(vec![
            Reference {
                reference: "refs/heads/feature-a".to_string(),
                url: "https://api.github.com/repos/octocat/Hello-World/git/refs/heads/feature-a".to_string(),
                object: Object {
                    object_type: "commit".to_string(),
                    sha: "aa218f56b14c9653891f9e74264a383fa43fefbd".to_string(),
                    url: "https://api.github.com/repos/octocat/Hello-World/git/commits/aa218f56b14c9653891f9e74264a383fa43fefbd".to_string(),
                },
            },
            Reference {
                reference: "refs/heads/feature-b".to_string(),
                url: "https://api.github.com/repos/octocat/Hello-World/git/refs/heads/feature-b".to_string(),
                object: Object {
                    object_type: "commit".to_string(),
                    sha: "612077ae6dffb4d2fbd8ce0cccaa58893b07b5ac".to_string(),
                    url: "https://api.github.com/repos/octocat/Hello-World/git/commits/612077ae6dffb4d2fbd8ce0cccaa58893b07b5ac".to_string(),
                },
            },
        ]);
        test_deserializing(payload, expected)
    }

    #[test]
    fn serialize_create_a_commit() {
        let original = CommitOptions::builder(
            "my commit message",
            "827efc6d56897b048c772eb4087f854f46256132",
            vec!["7d1b31e74ee336d15cbd21741bc88a537ed063a0"],
        ).author(
            "Scott Chacon",
            "schacon@gmail.com",
            "2008-07-09T16:13:30+12:00",
        )
            .build();
        let expected =
            concat!(
            "{",
            r#""message":"my commit message","#,
            r#""author":{"#,
            r#""name":"Scott Chacon","#,
            r#""email":"schacon@gmail.com","#,
            r#""date":"2008-07-09T16:13:30+12:00""#,
            "},",
            r#""parents":["#,
            r#""7d1b31e74ee336d15cbd21741bc88a537ed063a0""#,
            "],",
            r#""tree":"827efc6d56897b048c772eb4087f854f46256132""#,
            "}",
        );
        test_serializing(original, expected)
    }
}
