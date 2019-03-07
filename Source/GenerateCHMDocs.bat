rm -rf temp && ^
rm -rf ../DocsCHM && ^
mkdir temp && ^
pasdoc -Etemp --auto-link --visible-members strictprivate,private,strictprotected,protected,public,published,automated --sort=structures,constants,functions,types,variables,uses-clauses,record-fields,non-record-fields,methods,properties --format=htmlhelp -N LightFileStream LightFileStream.pas && ^
cd temp && ^
chmcmd --html-scan --verbosity 0 LightFileStream.hhp && ^
cd .. && ^
mkdir ..\DocsCHM && ^
mv temp/LightFileStream.chm ../DocsCHM/LightFileStream.chm && ^
rm -rf temp