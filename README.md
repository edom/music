# Music production workflow

## Plans

We build various software and install them to the `install` directory.

Use zynaddsubfx virtual keyboard layout for MIDI input
(jack-keyboard already does this)

## Usage

```
./enter
```

See [bashrc](bashrc) for available commands.

## Jackd2

[jackaudio/jack2/master...edom/jack2/master diff](https://github.com/jackaudio/jack2/compare/master...edom:master)

Issues:

- [295](https://github.com/jackaudio/jack2/issues/295)
- [181](https://github.com/jackaudio/jack2/issues/181), [182](https://github.com/jackaudio/jack2/pull/182)

### Build jackd2

```
# Copy the source code.
git clone https://github.com/jackaudio/jack2.git

# Install dependencies.
sudo apt-get install libsamplerate0-dev

# Build jack.
cd jack2
./waf configure --prefix=$(pwd)/../install/ --autostart=none --alsa=yes --samplerate=yes --debug
```

Run jackd:

```
LD_LIBRARY_PATH=install/lib/ ./install/bin/jackd --verbose --name jack-server-0 \
--realtime --realtime-priority 89 \
-d alsa --device hw:CARD=HDMI --playback --capture \
--rate 48000 --period 128 --nperiods 3
```

Alternative: buy a laptop (it doesn't have to be expensive) and install KXStudio?

### Build jack-keyboard

```
cd jack-keyboard-2.7.1
cmake -DCMAKE_INSTALL_PREFIX:PATH=$(pwd)/../../install/ -DLashEnable:BOOL=OFF ..
```

### Parametric equalizer

A parametric equalizer array is vital for acceptable headphone experience.

#### EQ10Q

#### Steve Harris LADSPA Single Band Parametric Equalizer (singlePara, LADSPA plugin number 1203)

Can't build.
Should we use EQ10Q instead?

Download a [gettext](https://www.gnu.org/software/gettext/) source package newer than 0.19.3,
or clone the Git repository if you want to.

Doesn't build.
`autoreconf -i` exits with error.
Complains about possibly undefined macros.
Is the error in the `configure.ac`?
``

```
git clone https://github.com/swh/ladspa.git swh-ladspa
```

## Running

```
./run
```

### Testing kernel realtimeness

[Cyclictest tool](https://wiki.linuxfoundation.org/realtime/documentation/howto/tools/cyclictest)

[How to understand cyclictest output](http://events.linuxfoundation.jp/sites/events/files/slides/cyclictest.pdf)

```
sudo apt-get install rt-tests

cyclictest -p 89 -D 60
```

And then load your system while cyclictest is running.

Look at the Max. It's in microseconds.

[Making Linux do hard real-time](https://www.slideshare.net/jserv/realtime-linux)

# This repository

Write a digital audio workstation in Scheme using language-oriented programming approach.

This license is Apache Software License version 2 or later.

Note: guile-gnome2 and guile-gtk are under the GPL.

ncurses

- [libsndfile](http://www.mega-nerd.com/libsndfile/) (LGPL 2.1 or LGPL 3)
    - [how to use](http://www.mega-nerd.com/libsndfile/api.html)

libsamplerate

Always keep `native.c` and `native.scm` synchronized.

If you use GTK2,
anticipate the [Gnome 3 porting guide](https://developer.gnome.org/Gnome3PortingGuide/).

## Building

Download [Guile ncurses](https://www.gnu.org/software/guile-ncurses/)

```
cc native.c -lsndfile
```

```
cd guile-ncurses-2.2
./configure --prefix=$(pwd)/../install --with-gnu-filesystem-hierarchy
# Workaround for bug in the build script?
compile() { guild compile "$@"; }
export -f compile
make -j4 install
```
