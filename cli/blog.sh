#/bin/bash

function usage() {
    echo <<EOF
Usage blog [--post|--delete|--update] [filename.md]

Options
--post   -p  Post's a new post to the blog. Error's if already exists.
--delete -d  Delete's a post from the blog. This is still correctable
             using git.
--update -u  Update's an existing blog post by replacing it with the 
             new one.
EOF
}
