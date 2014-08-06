#!/bin/bash
#This is a helper script to automatically generate a template for the weekly journal entry

#get commits since last journal update
LAST_UPDATE="$(git log -n1 --pretty=format:%H Journal.txt)"
LAST_UPDATE="${LAST_UPDATE:+$LAST_UPDATE..HEAD}"

#split existing journal at boundary
sed '/^=\+$/,$d' Journal.txt | sed -z 's/\n*$//' > .discussion
sed -n '/^=\+$/,$p' Journal.txt > .footer

#build up new journal
echo '================================================================================' >> new_journal
date '+%A %d %B %Y' >> new_journal

#use prefix of old journal as discussion, if it was present
if [[ $(stat -c '%s' .discussion) == "0" ]]; then
    echo -n "<INSERT DISCUSSION HERE>" >> new_journal
else
    cat .discussion >> new_journal
fi

#recent git commits
echo -e "\n\nGit commits:" >> new_journal
git log --pretty=format:"%h  %ad  %s" --date=iso $LAST_UPDATE >> new_journal

echo -e "\n" >> new_journal
cat .footer >> new_journal

#cleanup
mv new_journal Journal.txt
rm .discussion .footer

#render Lyx/Latex files to PDF
lyx -e pdf docs/Requirements\ Analysis/Requirements\ Analysis.lyx

cd docs/Design\ Specification/
latexmk Design_Specification.tex </dev/null
cd -

#commit changes
vim Journal.txt
git add Journal.txt
git commit -m "Journal updated"
