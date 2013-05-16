#!/bin/bash

#
# install.sh
#  LorikeeM Installer
# 
# Copyright (C) 2013 Coherent Logic Development
# 
# Provided under the terms of the GNU Affero General 
# Public License V3
#

export installer_caption="LorikeeM M Developer Tools"

function welcome_message
{
    installer_text="Welcome to the LorikeeM M Developer Tools installer."
    installer_text="${installer_text}\n\n"
    installer_text="${installer_text}This program will help you install "
    installer_text="${installer_text}LorikeeM onto your computer.\n\n"
    installer_text="${installer_text}LorikeeM requires FIS GT.M and "
    installer_text="${installer_text}GNU Emacs 23 or newer to function.\n"
    installer_text="${installer_text}You appear to be running ${DISTRIB_ID}.\n\n"
    installer_text="${installer_text}Choose OK to continue."

    whiptail --title "${installer_caption}" --msgbox "${installer_text}" 15 75
}

function check_distro 
{
    DISTRIB_ID=`lsb_release -i -s`
    case ${DISTRIB_ID} in 
	Ubuntu)
	    export PACKAGE_MANAGER="apt-get install"
	    ;;
	Debian)
	    export PACKAGE_MANAGER="apt-get install"
	    ;;
	Fedora)
	    export PACKAGE_MANAGER="yum install"
	    ;;
	CentOS)
	    export PACKAGE_MANAGER="yum install"
	    ;;
    esac
    export DISTRIB_ID
}

function check_for_whiptail 
{
    which whiptail > /dev/null
    if [ $? != 0 ]; then
	echo "This installer requires whiptail(1) to function."
	exit
    fi
}

function check_for_root
{
    if [ $EUID != 0 ]; then
	whiptail --title "${installer_caption}" --msgbox "You must have root privileges in order to install LorikeeM" 12 75
	exit
    fi
}


function check_for_emacs 
{
    which emacs > /dev/null
    if [ $? != 0 ]; then
	whiptail --title "${installer_caption}" \
	         --yesno "GNU Emacs does not appear to be installed. Do you wish to install it?" 12 75
	case $? in
	    0)
		sudo ${PACKAGE_MANAGER} emacs
		;;
	    1)
		echo "Please install GNU Emacs 23 or later and try again."
		exit
		;;
	esac
    fi
}

function check_create_directories
{
    whiptail --title "${installer_caption}" \
	     --infobox "Creating directories..." 12 75

    if [ ! -d "${HOME}/.emacs.d" ]; then
	mkdir ${HOME}/.emacs.d
    fi

    if [ ! -d "${HOME}/.emacs.d/plugins" ]; then
	mkdir ${HOME}/.emacs.d/plugins
    fi

    if [ ! -d "${HOME}/.emacs.d/plugins/lorikeem" ]; then
	mkdir ${HOME}/.emacs.d/plugins/lorikeem
    fi

    if [ ! -d "${HOME}/bin" ]; then
	mkdir ${HOME}/bin
    fi

    if [ ! -d "${HOME}/man/man1" ]; then
	mkdir -p ${HOME}/man/man1
    fi

    if [ ! -f "${HOME}/.emacs" ]; then
	touch ${HOME}/.emacs
    fi
}

function backup_file
{
    BACKUP_TIMESTAMP=`date +%Y%m%d%H%M%S`
    BACKUP_EXTENSION="lorikeem.${BACKUP_TIMESTAMP}"
    if [ -f "$1" ]; then
	mv $1 $1.${BACKUP_EXTENSION}
    fi
}

function copy_lorikeem_elisp
{
    whiptail --title "${installer_caption}" \
	     --infobox "Copying LorikeeM..." 12 75
   
    backup_file ${HOME}/.emacs.d/plugins/lorikeem/lorikeem.el

    cp src/lorikeem/lorikeem.el ${HOME}/.emacs.d/plugins/lorikeem/
}

function copy_yasnippet
{
    whiptail --title "${installer_caption}" \
	     --infobox "Installing YASnippet..." 12 75


    if [ -d "${HOME}/.emacs.d/plugins/yasnippet" ]; then
	whiptail --title "${installer_caption}" \
	         --yesno "YASnippet appears to have been previously installed.\n\nDo you wish to back up the current version?" 12 75

	if [ $? == 0 ]; then
	    BACKUP_TIMESTAMP=`date +%Y%m%d%H%M%S`
	    BACKUP_FILE="${HOME}/.emacs.d/plugins/yasnippet_${BACKUP_TIMESTAMP}.tar"
	    tar cvf ${BACKUP_FILE} ${HOME}/.emacs.d/plugins/yasnippet	    
        fi
	
	rm -rf ${HOME}/.emacs.d/plugins/yasnippet
	
    fi
    cp -r src/yasnippet ${HOME}/.emacs.d/plugins
}

function parse_gtmroutines
{
    local DIR DIRS

    for DIR in ${gtmroutines}
    do
	if [[ ${DIR} != ${DIR/"("/} ]] 
	then
	    DIR=`echo ${DIR} | cut -d "(" -f 2 | cut -d ")" -f 1`
	elif [[ ${DIR} != ${DIR/")"/} ]] 
	then
	    DIR=`echo ${DIR} | cut -d "(" -f 2 | cut -d ")" -f 1`
	elif [ "${DIR: -3}" == ".so" ]
	then
	    DIR=`dirname ${DIR}`
	elif ! ls ${DIR}/*.m &> /dev/null
	then
	    DIR=`dirname ${DIR}`
	fi
	DIRS="${DIRS} ${DIR}"
    done
    
    export ROUTINE_DIRS=${DIRS}
}

function choose_routine_path
{
    parse_gtmroutines

    whiptail_cmd="--title \"${installer_caption}\" --noitem --menu \"Where would you like to install Lorikeem's MUMPS routines?\" 15 76 4 "

    for DIR in ${ROUTINE_DIRS}
    do
	whiptail_cmd="${whiptail_cmd} \"${DIR}\" \" \""	
    done   

    whiptail_cmd="${whiptail_cmd} \"Other\" \" \""

    export CHOSEN_ROUTINE_PATH=$(eval whiptail ${whiptail_cmd} 3>&1 1>&2 2>&3)	

    if [ ${CHOSEN_ROUTINE_PATH} == "Other" ]; then
	export CHOSEN_ROUTINE_PATH=$(whiptail --inputbox "Custom MUMPS routine location:" 8 78 ${HOME} --title "${installer_caption}" 3>&1 1>&2 2>&3)
	if [ ! -d "${CHOSEN_ROUTINE_PATH}" ]; then
	    mkdir "${CHOSEN_ROUTINE_PATH}"
	fi
    fi

    if [ $? != 0 ]; then
	exit
    fi
}

function copy_mumps_routines
{
    choose_routine_path

    cp src/mumps/*.m ${CHOSEN_ROUTINE_PATH} 
}

function copy_manpages
{
    whiptail --title "${installer_caption}" \
	     --infobox "Installing documentation..." 12 75

    cp -p doc/mktags.1 doc/lorikeem.1 doc/KBAWDUMP.1 ${HOME}/man/man1/
    gzip -f9 ${HOME}/man/man1/{mktags,KBAWDUMP,lorikeem}.1

    if [ "${OS}" == "Ubuntu" -o "${OS}" == "LinuxMint" ]; then
	export MANPATH=:~/man
	mandb -q
    else
	if grep -q MANPATH ~/.bashrc
	then
	    # do nothing
	    echo "do nothing" > /dev/null
	else
	    echo >> ~/.bashrc
	    echo "export MANPATH=\$HOME/man:`manpath`" >> ~/.bashrc
	fi
    fi    
}

function update_emacs_config
{
    whiptail --title "${installer_caption}" \
	     --infobox "Updating Emacs configuration..." 12 75

    cat src/lorikeem/lorikeem-dotemacs.el >> ${HOME}/.emacs
}

function copy_binaries
{
    whiptail --title "${installer_caption}" \
	     --infobox "Installing binaries..." 12 75
   
    cp bin/* ${HOME}/bin
}

function update_search_path
{
    whiptail --title "${installer_caption}" \
	     --infobox "Updating search path..." 12 75

    cat src/lorikeem/lorikeem-path.sh >> ${HOME}/.bashrc
}

function install_crontab
{
    whiptail --title "${installer_caption}" \
             --infobox "Setting up scheduled tasks..." 12 75

    crontab -l > tmp_cron
    echo "0 * * * * mktags -e -g" >> tmp_cron
    crontab tmp_cron
    rm tmp_cron
}

function build_tags
{
    whiptail --title "${installer_caption}" \
             --infobox "Building ${HOME}/.MTAGS for routine cross-referencing..." 12 75

    ${HOME}/bin/mktags -e -g
}

function done_message 
{
    installer_text="LorikeeM has been successfully installed.\n\n"
    installer_text="${installer_text}You may start LorikeeM by typing\n\nlorikeem [filespec]\n\n"
    installer_text="${installer_text}at the shell prompt."
    whiptail --title "${installer_caption}" \
	     --msgbox "${installer_text}" 15 75
}

check_for_whiptail
check_distro
welcome_message
check_for_emacs
copy_mumps_routines
check_create_directories
copy_manpages
update_emacs_config
copy_lorikeem_elisp
copy_yasnippet
copy_binaries
update_search_path
install_crontab
build_tags
done_message