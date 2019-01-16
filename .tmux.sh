# TMUX Configurations
function tmux-python()
{
    # Starts a python IDE using tmux.
    #
    # Arugments:
    #   name (opt)     : the name of the session. defaults to "python ide" if
    #                    not provided
    #   startcmd (opt) : a shell command to run on startup in each pane.
    #
    # Result:
    #     +-------+-----------------------------+
    #     |   i   |                             |
    #     |   p   |                             |
    #     |   y   |            vim              |
    #     |   t   |                             |
    #     |   h   |                             |
    #     |   o   +-----------------------------|
    #     |   n   |          terminal           |
    #     +-------+-----------------------------+

    if [[ $# -gt 2 ]]; then
        echo "Too many arguments"
        return
    fi

    session_name=${1:-"python ide"}
    start_cmd=${2:-""}
    startdir=$(pwd)

    tmux start-server

    # start session with the given name and a main window called 'ide'.
    # Switch to the current directory and start vim
    tmux new -d -s ${session_name} -n ide

    # split the window
    tmux split-window -h -p 75 -c ${startdir}

    # select the leftmost pane and start ipython
    tmux select-pane -t 0
    tmux send-keys ${start_cmd} C-m
    tmux send-keys "ipython3" C-m

    # select the rightmost pane and start vim
    tmux select-pane -t 1
    tmux send-keys ${start_cmd} C-m
    tmux send-keys "vim" C-m

    # split horizontally 20%
    tmux split-window -v -p 20

    tmux select-pane -t 2
    tmux send-keys ${start_cmd} C-m
    tmux send-keys "clear" C-m

    # give vim pane focus
    tmux select-pane -t 1

    tmux attach-session -t ${session_name}
}


function tmux-cpp()
{
    # Starts a C++ IDE using tmux.
    #
    # Arugments:
    #   name (opt)     : the name of the session. defaults to "c++ ide" if
    #                    not provided
    #   startcmd (opt) : a shell command to run on startup in each pane.
    #
    # Result:
    #     +-----------------------------+
    #     |                             |
    #     |                             |
    #     |            vim              |
    #     |                             |
    #     |                             |
    #     +-----------------------------|
    #     |          terminal           |
    #     +-----------------------------+

    if [[ $# -gt 2 ]]; then
        echo "Too many arguments"
        return
    fi

    session_name=${1:-"c++ ide"}
    start_cmd=${2:-""}
    startdir=$(pwd)

    tmux start-server

    # start session with the given name and a main window called 'ide'.
    # Switch to the current directory and start vim
    tmux new -d -s ${session_name} -n ide

    # split the window
    tmux split-window -v -p 20 -c ${startdir}

    # select the topmost pane and start ipython
    tmux select-pane -t 0
    tmux send-keys ${start_cmd} C-m
    tmux send-keys "vim" C-m

    # select the bottommost pane
    tmux select-pane -t 1
    tmux send-keys ${start_cmd} C-m
    tmux send-keys "clear" C-m

    # give vim pane focus
    tmux select-pane -t 0

    tmux attach-session -t ${session_name}
}
