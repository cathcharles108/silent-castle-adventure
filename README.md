# Functional Adventure: The Silent Castle
This is a text-based RPG adventure game written entirely in the lovely functional language of Haskell.

## Running the game

If you are on a Windows 10 device, you can (potentially) run this game by simply downloading the `silent-castle-adventure-exe.exe` file, instead of the whole repo.

Otherwise, to run this game, you will need to have a way to configure Haskell. Since my professor used `stack`, I recommend doing it as well.

If you already have `stack` on your machine, go ahead and clone this repository, then run the following on your terminal:

    $ stack build

This may output several warnings, but as long as there is no error message it should be good to go.

Then, run the following:

    $ stack run

Once you see "Welcome to Functional Adventure: the Silent Castle!" then you have successfully run the game. Have fun!

If you do not have `stack`, please consult the following `stack` installation guide (taken from the MPCS 51400 Functional Programming course at the University of Chicago).

## Installing `stack`

### Windows or Linux

For Windows or Linux, try installing `stack` manually, following these instructions:

https://docs.haskellstack.org/en/stable/README/#how-to-install

'Installing manually' means using an installer specifically for `stack`, rather than using a package manager. If you don't know what a package manager is, don't worry about it! You're fine if you follow the above instructions.

### macOS

If you're on macOS, `homebrew` finally supports Haskell Stack. We recommend installing XCode Developer Utilities, which you can install by running this command:
    
    $ xcode-select --install

We also recommend installing `homebrew`, which you can do by following the instructions at the top of this page:

https://brew.sh/

You can then install the Haskell Stack platform with this command:

    $ brew install haskell-stack
Finally, you may need to update `gcc`, which you can do using `homebrew`.

### If all else fails, use the CS Linux cluster

If you get stuck in config hell getting `stack` to work on your machine, don't waste too much time on it. In that situation, we recommend connecting to the Linux cluster like so:

    $ ssh YOURCNETID@linux.cs.uchicago.edu
You will then be prompted for your cnetid and cnet password. This will open a remote login session on the Linux cluster, where `stack` is already set up for you to use.
