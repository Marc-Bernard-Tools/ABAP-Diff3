# ABAP Differ

Highlight the content difference between two text or HTML blocks

Made by [Marc Bernard Tools](https://marcbernardtools.com/) giving back to the [SAP Community](https://community.sap.com/)

NO WARRANTIES, [MIT License](LICENSE)

- This is a port of JavaScript (https://github.com/alaorneto/htmldiffer, no license defined)
- which is a port of CoffeeScript (https://github.com/tnwinc/htmldiff.js, MIT license)
- which is a port of the original Ruby (https://github.com/myobie/htmldiff, MIT license)

An enhancement was made so the code can also produce the diff of two texts (tags are treated like text).

## Prerequisites

None.

## Installation

You can install ABAP HTML Differ using [abapGit](https://github.com/abapGit/abapGit) either creating a new online repository for https://github.com/Marc-Bernard-Tools/ABAP-HTML-Differ or downloading the repository [ZIP file](https://github.com/Marc-Bernard-Tools/ABAP-HTML-Differ/archive/main.zip) and creating a new offline repository.

We recommend using package `$HTMLDIFF`.

## Usage

### HTML Diff

The following produces the diff of two example HTML snippets:

```abap
DATA:
  lv_original    TYPE string,
  lv_modified    TYPE string,
  lv_diff        TYPE string,
  lo_abap_differ TYPE REF TO zcl_abap_differ.

lv_original = '\n'
  && '    <p>First paragraph.</p>\n'
  && '    <ul>\n'
  && '        <li>Item A</li>\n'
  && '        <li>Item B</li>\n'
  && '        <li>Item C</li>\n'
  && '    </ul>\n'
  && '    <img src="previous.jpg">\n'
  && '    <span>This is some interesting text.</span>\n'.


lv_modified = '\n'
  && '    <p>First paragraph.</p>\n'
  && '    <ul>\n'
  && '        <li>Item A</li>\n'
  && '        <li>Item B</li>\n'
  && '        <li>Item D</li>\n'
  && '    </ul>\n'
  && '    <img src="next.jpg">\n'
  && '    <span>This is some new text.</span>\n'.

REPLACE ALL OCCURRENCES OF '\n' IN lv_original WITH cl_abap_char_utilities=>newline.
REPLACE ALL OCCURRENCES OF '\n' IN lv_modified WITH cl_abap_char_utilities=>newline.
  
CREATE OBJECT lo_differ.
  
lv_diff = lo_differ->htmldiff(
  iv_before   = lv_original
  iv_after    = lv_modified
  iv_with_img = abap_false ).
```

Result:

```html
    <p>First paragraph.</p>
    <ul>
        <li>Item A</li>
        <li>Item B</li>
        <li>Item <del>C</del><ins>D</ins></li>
    </ul>
    <img src='previous.jpg'><img src='next.jpg'>
    <span>This is some <del>interesting</del><ins>new</ins> text.</span>
```

By setting `iv_with_img = abap_true`, you can also mark changed images as deletions or insertions:

```abap
lv_diff = lo_differ->htmldiff(
  iv_before   = lv_original
  iv_after    = lv_modified
  iv_with_img = abap_true ).
```  

Result:

```html
    <p>First paragraph.</p>
    <ul>
        <li>Item A</li>
        <li>Item B</li>
        <li>Item <del>C</del><ins>D</ins></li>
    </ul>
    <del><img src='previous.jpg'></del><ins><img src='next.jpg'></ins>
    <span>This is some <del>interesting</del><ins>new</ins> text.</span>
```

## Contributions

All contributions are welcome! Just fork this repo and create a pull request. 

## About

<p>Made with :heart: in Canada</p>
<p>Copyright © 2021 <a href="https://marcbernardtools.com/">Marc Bernard Tools</a></p>
<p>Follow <a href="https://twitter.com/marcfbe">@marcfbe</a> on Twitter</p>
<p><a href="https://marcbernardtools.com/"><img width="160" height="65" src="https://marcbernardtools.com/info/MBT_Logo_640x250_on_Gray.png" alt="MBT Logo"></a></p>

