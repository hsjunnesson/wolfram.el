# Wolfram Alpha integration in Emacs

Allows you to query Wolfram Alpha from within Emacs.

## Installation

Download `wolfram.el` to a directory in your `load-path`. Then add this to your init file:

```
(require 'wolfram)
```

## Usage

To make a query run `M-x wolfram-alpha` then type your query. It will show the result in an org-mode buffer called `*WolframAlpha*`.

## Examples

*How much does Lake Victoria weigh?*

![How much does Lake Victoria weigh?](https://s3.amazonaws.com/wolfram.el/query.png)

*How many nukes does the United State have?*

![How many nukes does the United State have?](https://s3.amazonaws.com/wolfram.el/plot.png)
