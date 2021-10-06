# ABAP HTML Differ

Highlight the content difference between two HTML blocks

Made by [Marc Bernard Tools](https://marcbernardtools.com/) giving back to the [SAP Community](https://community.sap.com/)

NO WARRANTIES, [MIT License](LICENSE)

`
This  is a port of JavaScript        (https://github.com/alaorneto/htmldiffer, no license)
which is a port of CoffeeScript      (https://github.com/tnwinc/htmldiff.js, MIT license)
which is a port of the original Ruby (https://github.com/myobie/htmldiff, MIT license)
`

## Prerequisite

None.

## Installation

You can install ABAP HTML Differ using [abapGit](https://github.com/abapGit/abapGit) either creating a new online repository for https://github.com/Marc-Bernard-Tools/ABAP-HTML-Differ or downloading the repository [ZIP file](https://github.com/Marc-Bernard-Tools/ABAP-HTML-Differ/archive/main.zip) and creating a new offline repository.

We recommend to use package `$HTMLDIFF`.

## Usage

### ABAP

```abap
CONSTANTS:
  c_cr TYPE string VALUE cl_abap_char_utilities=>cr_lf.
  
DATA:
  lv_original   TYPE string,
  lv_modified   TYPE string,
  lo_htmldiffer TYPE REF TO zcl_html_differ.

lv_original = '' 
  && c_cr && '    <p>First paragraph.</p>'
  && c_cr && '    <ul>'
  && c_cr && '        <li>Item A</li>'
  && c_cr && '        <li>Item B</li>'
  && c_cr && '        <li>Item C</li>'
  && c_cr && '    </ul>'
  && c_cr && '    <img src="previous.jpg">'
  && c_cr && '    <span>This is some interesting text.</span>'
  && c_cr.
  
lv_modified = ''
  && c_cr && '    <p>First paragraph.</p>'
  && c_cr && '    <ul>'
  && c_cr && '        <li>Item A</li>'
  && c_cr && '        <li>Item B</li>'
  && c_cr && '        <li>Item D</li>'
  && c_cr && '    </ul>'
  && c_cr && '    <img src="next.jpg">'
  && c_cr && '    <span>This is some new text.</span>'
  && c_cr.
  
lv_diff = mo_htmldiffer->diff(
  iv_before   = lv_original
  iv_after    = lv_modified
  iv_with_img = abap_false ).
```

### Output

`lv_diff` will contain the following result:

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

If you call with `iv_with_img = abap_true`, the result will be as following:

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

