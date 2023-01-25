#lang scribble/manual

@(require "common.rkt")

@(define releases-page "https://github.com/benknoble/frosthaven-manager/releases")

@title{Installing Frosthaven Manager}

You can grab the latest version of Frosthaven Manager at the
@link[releases-page]{release page}. Download the release for your operating
system (Windows, Linux, or macOS) and follow the instructions below.

If you get stuck, refer to @secref{Troubleshooting} for help.

@margin-note{The following instructions use the terms "directory" and "folder"
interchangeably. They are the same thing, and you can mentally substitute
whichever is more comfortable for you.}

@section{Windows}

First, open the downloaded archive in the File Explorer. Double-click the
archive to open it, which should uncompress the archive. You may wish to
uncompress into a directory of your choosing by right-clicking and selecting the
appropriate option.

Among the uncompressed files you will find the application
@filepath{FrosthavenManager.exe} and @filepath{lib} folder needed by the
program. Click the @filepath{FrosthavenManager.exe} to play.

@section{Linux}

Extract the compressed archive via the usual method. For example, the following
command-line should do:

@terminal|{
gunzip -c /path/to/linux-FrosthavenManager.tar.gz | tar xvf -
}|

Inside the uncompressed archive should be an executable program and supporting
libraries. Run the program to play.

@section{macOS}

First, open the downloaded archive in Finder. Double-click the archive to open
it, which should uncompress files into a directory named
@filepath{FrosthavenManager}.

In the @filepath{FrosthavenManager} directory, you will find the application
@filepath{FrosthavenManager.app}, which you can drag to your
@filepath{Applications} folder.

To open the Frosthaven Manager for the first time, you will need to right-click
the app and select @onscreen{Open}. After agreeing to trust the app by clicking
@onscreen{Open} again, you should see the application open. Next time, click on
the app to launch it as normal!
