rm -rf temp && ^
rm -rf ../DocsHTML && ^
mkdir temp && ^
pasdoc -Etemp --auto-link --use-tipue-search --visible-members strictprivate,private,strictprotected,protected,public,published,automated --sort=structures,constants,functions,types,variables,uses-clauses,record-fields,non-record-fields,methods,properties --format=html -N LightFileStream LightFileStream.pas && ^
mv temp ../DocsHTML && ^
rm -rf temp