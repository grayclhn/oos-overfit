# This file is a simplified version of
# <https://github.com/JuliaLang/julia/blob/95ee26932b9cd0ed748e18853c6bf5de769edb06/base/version_git.sh>
# which is a part of Julia. License is MIT:
# <http://julialang.org/license>

last_tag=$(git describe --tags --abbrev=0)
git_time=$(git log -1 --pretty=format:%ct)

#collect the contents
commit=$(git rev-parse HEAD)
commit_short=$(git rev-parse --short HEAD)
if [ -n "$(git status --porcelain)" ]; then
    # append dirty mark '*' if the repository has uncommited changes
    commit_short="$commit_short"*
fi
branch=$(git branch | sed -n '/\* /s///p')
# Some versions of wc (eg on OS X) add extra whitespace to their output.
# The sed(1) call stops this from breaking the generated Julia's indentation by
# stripping all non-digits.
build_number=$(git rev-list HEAD ^$last_tag | wc -l | sed -e 's/[^[:digit:]]//g')
# Check for errrors and emit default value for missing numbers.
if [ -z "$build_number" ]; then
    build_number="0"
fi

date_string=$git_time
case $(uname) in
  Darwin | FreeBSD)
    date_string="$(/bin/date -jr $git_time -u '+%Y-%m-%d')"
    ;;
  MINGW*)
    git_time=$(git log -1 --pretty=format:%ci)
    date_string="$(/bin/date --date="$git_time" -u '+%Y-%m-%d')"
    ;;
  *)
    date_string="$(/bin/date --date="@$git_time" -u '+%Y-%m-%d')"
    ;;
esac

if [ $(git describe --tags --exact-match 2> /dev/null) ]; then
    echo $(git describe --tags --exact-match)
else
    echo "$date_string, commit $commit_short"
fi
