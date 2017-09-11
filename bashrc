if [ -f ~/.bashrc ]; then
    source ~/.bashrc
fi

PS1="(enter)$PS1"

# Configure
export configure_prefix="$(pwd)/install/"

# Linker
export LD_LIBRARY_PATH="${configure_prefix}lib/:$LD_LIBRARY_PATH"

# Bash
export PATH="$(pwd)/install/bin/:$PATH"

jack-install ()
(
    set -o errexit
    set -o pipefail
    set -o nounset
    cd jack2
    ./waf install "$@"
)

jack-run ()
{
    jackd --verbose \
        --realtime --realtime-priority 89 \
        -d alsa --device hw:CARD=HDMI --playback --capture \
        --rate 48000 --period 128 --nperiods 3 \
        "$@"
}

configure ()
{
    ./configure --prefix="$configure_prefix" "$@"
}

cgrep ()
{
    grep -rn \
        --include='*.c' --include='*.cc' --include='*.cpp' --include='*.cxx' --include='*.c++' \
        --include='*.h' --include='*.hh' --include='*.hpp' \
        "$@" .
}
