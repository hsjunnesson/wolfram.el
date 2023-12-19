[![MELPA](http://melpa.org/packages/wolfram-badge.svg)](http://melpa.org/#/wolfram)

# Wolfram Alpha integration in Emacs

Allows you to query Wolfram Alpha from within Emacs.

# Installation

Install package `wolfram` from MELPA or download `wolfram.el` to a directory in your `load-path`. Then add this to your init file:

```el
(require 'wolfram)
```

# Variables 

### Wolfram Alpha App ID
Create an account at [wolframalpha.com](http://www.wolframalpha.com), then in your account select "My Apps (API)".
Create a new AppID. In Emacs set that AppID as the custom variable `wolfram-alpha-app-id`.

### Optional Dark Mode
Enable dark mode by setting `wolfram-use-dark-version` to `t` in your configuration file. Dark mode requires ImageMagick's `convert` utility to invert images for a dark theme. Without ImageMagick installed, image inversion will not function, and a warning will be displayed if dark mode is enabled.

```el
(setq wolfram-use-dark-version t)
```

### Magnification Factor
Adjust the magnification factor for images in query results using the custom variable `wolfram-alpha-magnification-factor`. A higher value results in larger images.

```el
(setq wolfram-alpha-magnification-factor 1.5)
```

Set the factor according to your viewing preferences and display resolution.

## Usage

To make a query, run `M-x wolfram-alpha` then type your query. It will show the result in a buffer called `*WolframAlpha*`.

## Examples

*How much does Lake Victoria weigh?*

![How much does Lake Victoria weigh?](https://s3.amazonaws.com/wolfram.el/query.png)

*How many nukes does the United State have?*

![How many nukes does the United State have?](https://s3.amazonaws.com/wolfram.el/plot.png)

*Weather in Paris (dark-mode enabled)*

