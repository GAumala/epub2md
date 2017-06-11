# epub2md

[![Build Status](https://travis-ci.org/GAumala/epub2md.svg?branch=master)](https://travis-ci.org/GAumala/epub2md)

Haskell program that converts `.epub` files to Markdown from your terminal. 

## How it works

This program unzips the `.epub` file, then goes through each `.xhtml` file and converts it to markdown. The output files are placed in a new directory. File conversion is still limited, only a handful of XHTML tags are currently supported. The supported tags are:

- `h1` through `h6`
- `p`
- `ul`
- `li`
- `em`
- `i`
- `strong`

Anything else is either completely ignored (such as images), or only the inner text makes it to the markdown file. However, these tags should be enough for most novels stuff like that. PR's are welcome. 

## Dependencies 

[`unzip`](http://www.info-zip.org/UnZip.html) is required to extract the `.xhtml` files.  

## Install

Clone this repo and build with [Stack](https://docs.haskellstack.org/en/stable/GUIDE/)

```bash
git clone https://github.com/GAumala/epub2md
cd epub2md
stack setup
stack install
```

## Usage

Pass your EPUB file as argument

```
epub2md mybook.epub
```

After running that, you should have an `mybook-md` directory with a bunch of markdown files.

[![asciicast](https://asciinema.org/a/81ckjse0r5k0inx1m02pngl32.png)](https://asciinema.org/a/81ckjse0r5k0inx1m02pngl32)




