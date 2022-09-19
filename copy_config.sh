declare -A dict_files


paths_of_config_files=(
	"$HOME/.config/alacritty/alacritty.yml"
	"$HOME/.config/i3/config"
	"$HOME/.config/Thunar/uca.xml"
	"$HOME/.emacs"
	"$HOME/.xinitrc"
	"/etc/vimrc"
)

dict_files["alacritty.yml"]="$HOME/.config/alacritty/alacritty.yml"
dict_files["config"]="$HOME/.config/i3/config"
dict_files["uca.xml"]="$HOME/.config/Thunar/uca.xml"
dict_files[".emacs"]="$HOME/.emacs"
dict_files[".xinitrc"]="$HOME/.xinitrc"
dict_files["vimrc"]="/etc/vimrc"

current_files=( $(ls -a .) )

# If if we want to copy then copy
if [ "$1" == "copy" ]; then
	# Copy each file in paste it here 
	for file in ${paths_of_config_files[@]}; do
		# Check if the key exist 
		echo "Copying $file to $(pwd)" 
		cp $file .
	done
elif [ "$1" == "move" ]; then
	# copy and move each file to the correct path
	for file in ${current_files[@]}; do
		# Check if the key exist 
		if [ -v dict_files["$file"] ]; then
			echo "Moving $file to ${dict_files["$file"]}" 
			cp $file ${dict_files["$file"]}
		fi
	done
else
	# Input error
	echo "bad input try [copy/move]"
	echo "copy - to copy all the config files"
	echo "move - to set all config files into the correct path"
fi


