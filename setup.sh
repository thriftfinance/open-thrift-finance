#!/usr/bin/env bash

# Exit if any command throws an error
set -e

read -p "Enter the project name in lower dashed case (e.g. my-project): " project_name

expected_module_name=$(sed -r 's/(^|[-_ ]+)([0-9a-z])/\U\2/g' <<< "$project_name")
read -p "Enter the module name in upper camel case (e.g. MyProject) (default: ${expected_module_name}): " module_name
if [ -z "$module_name" ]
then
  module_name=$expected_module_name
fi

read -p "Enter the github org and repo name as my-repo/my-project): " github_input
new_github_link="https://github.com/$github_input"

while true; do
    read -p "Would you like to keep github runner actions? (Y/N): " yn
    case $yn in
        [Yy]* ) keep_actions=1; break;;
        [Nn]* ) keep_actions=0; break;;
        * ) echo "Please answer yes or no.";;
    esac
done

# Sed runs differently on Mac and Linux. Determine which one we're on
sed_command="sed -i "
if [[ $OSTYPE == 'darwin'* ]]
  then
    sed_command="sed -i'' "
fi

# Modify the cabal files
old_github_link="https://github.com/template-project"
for file in $(find -name "template-project*.cabal"); do
  new_cabal_file=$(echo $file | sed "s|template-project|${project_name}|g")
  mv $file $new_cabal_file
  git add $file
    
  $sed_command "s|${old_github_link}|${new_github_link}|g" $new_cabal_file
  $sed_command "s|template-project|${project_name}|g" $new_cabal_file
  $sed_command "s|TemplateProject|${module_name}|g" $new_cabal_file
  git add $new_cabal_file
done

# Modify flake.nix, Makefile, and integrate.yaml
$sed_command "s|template-project|${project_name}|g" flake.nix
$sed_command "s|template-project|${project_name}|g" Makefile
git add flake.nix Makefile

if [ "$keep_actions" -eq "1" ]
then
  $sed_command "s|template-project|${project_name}|g" ..github/workflows/integrate.yaml
  git mv ..github .github
else
  rm -rf ..github
  git add ..github
fi

# Modify the dummy source files and Spec files
for file in $(grep -rl TemplateProject ./*/); do
  $sed_command "s|TemplateProject|${module_name}|g" $file
  git add $file
done
for file in $(find -name "*TemplateProject*"); do
  new_file=$(echo $file | sed "s|TemplateProject|${module_name}|g")
  mv $file $new_file
  git add $file $new_file
done

# Commit to make sure nix is aware of renamed files
git commit -m "Ran setup.sh"

# Perform first build and test
nix develop .#onchain --command bash -c "cd onchain && cabal build && cabal test"
nix develop .#offchain --command bash -c "cd offchain && cabal build && cabal test"

# Perform CI actions
nix develop .#onchain --command make lint
nix develop .#onchain --command make format
nix develop .#offchain --command make format
nix build .#check.x86_64-linux

echo "Successfully renamed and built project. A commit containing the changes has already been added (but not pushed)."
echo "Happy coding!"
