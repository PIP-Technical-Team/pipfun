# Functions for managing release branches

``` r
library(pipfun)

setup_working_release(release = "20250203")
```

The following functions provide tools for managing branches for specific
PIP releases. These are mainly intended to be used for auxiliary data,
which are stored in dedicated repositories, such as aux_gdp for GDP data
or aux_ppp for PPP data. These functions are a starting point to enable
vintage control of auxiliary data, so we can track which version of aux
data was used for specific releases. They also ensure that release
branches are consistently updated with the latest changes from the
development branch.

The following workflow is recommended:

### Step 1 - Check if repo has release branch

As a first step, check whether any auxiliary data repo has or not a
release branch. Release branches are named after the corresponding day
of the release (e.g., 20241105 for November 5th 2024 release).

To do so, call
[`get_repo_branches()`](https://pip-technical-team.github.io/pipfun/reference/get_repo_branches.md).
It returns a list with (1.) names of all branches, (2.) name of release
branch if present and (3.) logical “has_release_branch” specifying
whether or not the repo has a release branch.

``` r

repo_branches <- get_repo_branches(repo = "aux_ppp",
                  owner = getOption("pipfun.ghowner"))

names(repo_branches)

# printing all elements of the list
repo_branches$all_branches
repo_branches$release_branch
repo_branches$has_release_branch
```

Let’s use a fake auxiliary data repo, aux_test, to illustrate how the
functions work.

### Step 2 - If absent, create release branch

To create a release branch, simply call
[`create_new_branch()`](https://pip-technical-team.github.io/pipfun/reference/create_new_branch.md),
specifying the name of the new branch as well as the name of the
reference branch it should be created from. If you would like to create
a new branch for the current release, you can just leave the new_branch
argument as `NULL` and the function will automatically create the new
branch of the release that’s been setup.

``` r

create_new_branch(repo = "aux_test",
                  ref_branch = "main"
                  )
```

### Step 3 - Check if release branch is updated with development branch

In order to check whether the release branch is updated with the most
recent version of development branch, call
[`compare_branch_content()`](https://pip-technical-team.github.io/pipfun/reference/compare_branch_content.md).
This function compares the content of the two branches based on their
corresponding latest commit tree SHAs. This is a string representing the
SHA hash of the tree object associated with the branch. In Git, a tree
represents the entire directory structure and content of a commit. The
SHA is a unique identifier based on the files and folders’ state in that
branch.

Therefore, comparing the SHA of the latest commits is not enough to
determine if branches have the same content because:

- Tree SHA: Represents the state of the repository’s content (files and
  directories) at a given point in time.
- Commit SHA: Includes metadata such as the author, timestamp, and
  commit message, in addition to referencing the tree SHA. Even if the
  content is identical, differences in metadata can cause the commit
  SHAs to differ.

As a result,
[`compare_branch_content()`](https://pip-technical-team.github.io/pipfun/reference/compare_branch_content.md)
returns a list with: (1) tree SHA of branch 1, (2) tree SHA of branch 2
and (3) a TRUE/FALSE value indicating whether the two branches have
identical content. If the tree SHAs for both branches are the same,
`same_content` will be `TRUE`; otherwise, it will be `FALSE`.

``` r

br_compare <- compare_branch_content(repo = "aux_test",
                                     branch1 = "20250203_TEST",
                                     branch2 = "DEV")

# Output list
names(br_compare)

br_compare$tree_sha_1
br_compare$tree_sha_2

br_compare$same_content

# Example with branches of the same content
compare_branch_content(repo    = "aux_test",
                       branch1 = "main",
                       branch2 = "test_main")
```

### Step 4 - Update branches

There are two functions that allow updating branches with two different
approaches:

1.  Update the target branch to point to the latest commit of the source
    branch (this overwrites the history of target branch).
2.  Update the target branch by merging the source branch into it.

``` r

# Approach 1:
update_branches(repo    = "aux_test",
                branch1 = "DEV_v2",
                branch2 = "20250203_TEST")


# Approach 2:
merge_branch_into(repo = "aux_test", 
                  source_branch = "DEV",
                  target_branch = "20250203_TEST")
```

In both cases, you can set the `force` option to `FALSE` to make the
function ask confirmation before proceeding with the operation.

### Additional: Delete branches

To delete a branch, you can use the
[`delete_branch()`](https://pip-technical-team.github.io/pipfun/reference/delete_branch.md)
function. Here’s an example that demonstrates creating and subsequently
deleting a branch:

``` r

# Create a new branch named "to_delete"
create_new_branch(repo = "aux_test",
                  new_branch = "to_delete")

# Delete the branch named "to_delete" (asks for confirmation in interactive mode)
delete_branch(repo = "aux_test",
              branch_to_delete = "to_delete")
```
