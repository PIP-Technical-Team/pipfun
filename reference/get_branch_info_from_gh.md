# Get info of a branch in a GitHub repo

Get info of a branch in a GitHub repo

## Usage

``` r
get_branch_info_from_gh(
  owner = getOption("pipfun.ghowner"),
  repo,
  branch = "main",
  gh_func = gh::gh,
  creds_func = get_github_creds,
  url_func = info_from_url
)
```

## Arguments

- owner:

  character: owner of repo

- repo:

  character: repository name

- branch:

  character: branch name (default is "main")

- gh_func:

  function: function used to call the GitHub API (default is
  [`gh::gh`](https://gh.r-lib.org/reference/gh.html))

- creds_func:

  function: function used to retrieve GitHub credentials (default is
  `get_github_creds`)

- url_func:

  function: function used to extract additional information from the
  protection URL (default is `info_from_url`)

## Value

Complete response from GET method of GitHub API

## Examples

``` r
get_branch_info_from_gh(owner     = getOption("pipfun.ghowner"),
                        repo      = "pip_info",
                        branch    = "releases")
#> Git credentials are missing or invalid in non-interactive mode.
#> $name
#> [1] "releases"
#> 
#> $commit
#> $commit$sha
#> [1] "027cc6fbe6b547167e6f21af2dfaebcc620c071d"
#> 
#> $commit$node_id
#> [1] "C_kwDOMtdcOdoAKDAyN2NjNmZiZTZiNTQ3MTY3ZTZmMjFhZjJkZmFlYmNjNjIwYzA3MWQ"
#> 
#> $commit$commit
#> $commit$commit$author
#> $commit$commit$author$name
#> [1] "RossanaTat"
#> 
#> $commit$commit$author$email
#> [1] "150610573+RossanaTat@users.noreply.github.com"
#> 
#> $commit$commit$author$date
#> [1] "2026-02-23T14:16:59Z"
#> 
#> 
#> $commit$commit$committer
#> $commit$commit$committer$name
#> [1] "RossanaTat"
#> 
#> $commit$commit$committer$email
#> [1] "150610573+RossanaTat@users.noreply.github.com"
#> 
#> $commit$commit$committer$date
#> [1] "2026-02-23T14:16:59Z"
#> 
#> 
#> $commit$commit$message
#> [1] "Updating data via R script on 2026-02-23 09:16:59.76218"
#> 
#> $commit$commit$tree
#> $commit$commit$tree$sha
#> [1] "14e1c3d965ad8817c84ed3e297a8d9bd8d2b043a"
#> 
#> $commit$commit$tree$url
#> [1] "https://api.github.com/repos/PIP-Technical-Team/pip_info/git/trees/14e1c3d965ad8817c84ed3e297a8d9bd8d2b043a"
#> 
#> 
#> $commit$commit$url
#> [1] "https://api.github.com/repos/PIP-Technical-Team/pip_info/git/commits/027cc6fbe6b547167e6f21af2dfaebcc620c071d"
#> 
#> $commit$commit$comment_count
#> [1] 0
#> 
#> $commit$commit$verification
#> $commit$commit$verification$verified
#> [1] FALSE
#> 
#> $commit$commit$verification$reason
#> [1] "unsigned"
#> 
#> $commit$commit$verification$signature
#> NULL
#> 
#> $commit$commit$verification$payload
#> NULL
#> 
#> $commit$commit$verification$verified_at
#> NULL
#> 
#> 
#> 
#> $commit$url
#> [1] "https://api.github.com/repos/PIP-Technical-Team/pip_info/commits/027cc6fbe6b547167e6f21af2dfaebcc620c071d"
#> 
#> $commit$html_url
#> [1] "https://github.com/PIP-Technical-Team/pip_info/commit/027cc6fbe6b547167e6f21af2dfaebcc620c071d"
#> 
#> $commit$comments_url
#> [1] "https://api.github.com/repos/PIP-Technical-Team/pip_info/commits/027cc6fbe6b547167e6f21af2dfaebcc620c071d/comments"
#> 
#> $commit$author
#> $commit$author$login
#> [1] "RossanaTat"
#> 
#> $commit$author$id
#> [1] 150610573
#> 
#> $commit$author$node_id
#> [1] "U_kgDOCPoijQ"
#> 
#> $commit$author$avatar_url
#> [1] "https://avatars.githubusercontent.com/u/150610573?v=4"
#> 
#> $commit$author$gravatar_id
#> [1] ""
#> 
#> $commit$author$url
#> [1] "https://api.github.com/users/RossanaTat"
#> 
#> $commit$author$html_url
#> [1] "https://github.com/RossanaTat"
#> 
#> $commit$author$followers_url
#> [1] "https://api.github.com/users/RossanaTat/followers"
#> 
#> $commit$author$following_url
#> [1] "https://api.github.com/users/RossanaTat/following{/other_user}"
#> 
#> $commit$author$gists_url
#> [1] "https://api.github.com/users/RossanaTat/gists{/gist_id}"
#> 
#> $commit$author$starred_url
#> [1] "https://api.github.com/users/RossanaTat/starred{/owner}{/repo}"
#> 
#> $commit$author$subscriptions_url
#> [1] "https://api.github.com/users/RossanaTat/subscriptions"
#> 
#> $commit$author$organizations_url
#> [1] "https://api.github.com/users/RossanaTat/orgs"
#> 
#> $commit$author$repos_url
#> [1] "https://api.github.com/users/RossanaTat/repos"
#> 
#> $commit$author$events_url
#> [1] "https://api.github.com/users/RossanaTat/events{/privacy}"
#> 
#> $commit$author$received_events_url
#> [1] "https://api.github.com/users/RossanaTat/received_events"
#> 
#> $commit$author$type
#> [1] "User"
#> 
#> $commit$author$user_view_type
#> [1] "public"
#> 
#> $commit$author$site_admin
#> [1] FALSE
#> 
#> 
#> $commit$committer
#> $commit$committer$login
#> [1] "RossanaTat"
#> 
#> $commit$committer$id
#> [1] 150610573
#> 
#> $commit$committer$node_id
#> [1] "U_kgDOCPoijQ"
#> 
#> $commit$committer$avatar_url
#> [1] "https://avatars.githubusercontent.com/u/150610573?v=4"
#> 
#> $commit$committer$gravatar_id
#> [1] ""
#> 
#> $commit$committer$url
#> [1] "https://api.github.com/users/RossanaTat"
#> 
#> $commit$committer$html_url
#> [1] "https://github.com/RossanaTat"
#> 
#> $commit$committer$followers_url
#> [1] "https://api.github.com/users/RossanaTat/followers"
#> 
#> $commit$committer$following_url
#> [1] "https://api.github.com/users/RossanaTat/following{/other_user}"
#> 
#> $commit$committer$gists_url
#> [1] "https://api.github.com/users/RossanaTat/gists{/gist_id}"
#> 
#> $commit$committer$starred_url
#> [1] "https://api.github.com/users/RossanaTat/starred{/owner}{/repo}"
#> 
#> $commit$committer$subscriptions_url
#> [1] "https://api.github.com/users/RossanaTat/subscriptions"
#> 
#> $commit$committer$organizations_url
#> [1] "https://api.github.com/users/RossanaTat/orgs"
#> 
#> $commit$committer$repos_url
#> [1] "https://api.github.com/users/RossanaTat/repos"
#> 
#> $commit$committer$events_url
#> [1] "https://api.github.com/users/RossanaTat/events{/privacy}"
#> 
#> $commit$committer$received_events_url
#> [1] "https://api.github.com/users/RossanaTat/received_events"
#> 
#> $commit$committer$type
#> [1] "User"
#> 
#> $commit$committer$user_view_type
#> [1] "public"
#> 
#> $commit$committer$site_admin
#> [1] FALSE
#> 
#> 
#> $commit$parents
#> $commit$parents[[1]]
#> $commit$parents[[1]]$sha
#> [1] "8ae72ed52debd4191bb6f7d381c71fc0fad8d47f"
#> 
#> $commit$parents[[1]]$url
#> [1] "https://api.github.com/repos/PIP-Technical-Team/pip_info/commits/8ae72ed52debd4191bb6f7d381c71fc0fad8d47f"
#> 
#> $commit$parents[[1]]$html_url
#> [1] "https://github.com/PIP-Technical-Team/pip_info/commit/8ae72ed52debd4191bb6f7d381c71fc0fad8d47f"
#> 
#> 
#> 
#> 
#> $`_links`
#> $`_links`$self
#> [1] "https://api.github.com/repos/PIP-Technical-Team/pip_info/branches/releases"
#> 
#> $`_links`$html
#> [1] "https://github.com/PIP-Technical-Team/pip_info/tree/releases"
#> 
#> 
#> $protected
#> [1] FALSE
#> 
#> $protection
#> $protection$enabled
#> [1] FALSE
#> 
#> $protection$required_status_checks
#> $protection$required_status_checks$enforcement_level
#> [1] "off"
#> 
#> $protection$required_status_checks$contexts
#> list()
#> 
#> $protection$required_status_checks$checks
#> list()
#> 
#> 
#> 
#> $protection_url
#> [1] "https://api.github.com/repos/PIP-Technical-Team/pip_info/branches/releases/protection"
#> 
#> $owner
#> [1] "PIP-Technical-Team"
#> 
#> $repo
#> [1] "pip_info"
#> 
#> $branch
#> [1] "releases"
#> 
```
