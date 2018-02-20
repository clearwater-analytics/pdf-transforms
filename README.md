# pdf-transforms
Pdf-transforms is a Clojure library for transforming PDF documents into data structures that are more amenable to machine processing.
It accomplishes this through the use of exceedingly clever (just don't consult the source code on this one) heuristics to detect and leverage 
important visual features of a document (e.g. whitespace columns & font changes) in order to detect and parse components such as tables and paragraphs.

Pdf-transforms uses the following data processing pipeline:
 * Call Apache's PDFBox library to obtain the raw data of the pdf (i.e. TextPositions, bookmarks, and drawn lines).
 * Compose word tokens from the raw data.
 * Compose blocks by grouping word tokens according to implicit document boundaries (e.g. whitespace)
 * Ornament blocks with features and a class
 * Compose components by grouping and then reformatting ornamented blocks
 

## Usage

To install, add the following to your project :dependencies :

    [pdf-transforms "0.1.0"]

Require the pdf-transforms.core in your namespace: 

    (ns my.ns 
        (:require [pdf-transforms.core :as pdft]))

### Examples

    ;;transform a local file via url
    (pdft/transform "file:///home/userX/Documents/my_doc.pdf")
      ;; output is a sequence of components (see below for the shape of a component)
           
    ;;transform via a File object
    ;(import java.io.File)
    (pdft/transform (File. "my_doc.pdf"))
       ;; output is a sequence of components (see below for the shape of a component)
        
    ;;transform the first 3 pages only
    (pdft/transform "file:///home/userX/Documents/my_doc.pdf"
                    {:page-bounds [0 3]})
        ;; output is a sequence of components (see below for the shape of a component)
                    
    ;;transform the first 3 pages as plain text
    (pdft/transform "file:///home/userX/Documents/my_doc.pdf"
                    {:page-bounds [0 3] :format :plain-text})
    ;; output is a text string
                    
    ;;annotating blocks (note that a url string must be used for annotate functions)
    (pdft/annotate-blocks "file:///home/userX/Documents/my_doc.pdf")
        
    ;;annotating components on certain pages, writing the resulting pdf to a specified directory
    (pdft/annotate-blocks "file:///home/userX/Documents/my_doc.pdf" {:page-bounds [0 2] :output-directory "/home/userX/Documents/annotated-pdfs"}) 
   
A sample pdf page, taken from the USDA ARS (https://www.ars.usda.gov/ARSUserFiles/oc/np/NutritiveValueofFoods/NutritiveValueofFoods.pdf), is included in the 
project resources, along with the results of annotating blocks and components on the page.
    
### Output formats
The form of different output formats is officially defined with clojure.spec in the pdf-transforms.spec namespace, 
but can be briefly summarized below.

#### Tokens
Most of pdf-transform's output formats are a composition of token elements, which have the form:

    {:text "Today",
     :x 309.7206,
     :y 753.3,
     :page-number 2,
     :f-size 12.0,
     :height 7.884,
     :width 6.0,
     :font "IKLOJE+TimesNewRomanPSMT",
     :id "2_309_753"}

Each token might also contain one or more of the following attributes:

     :bold? true
     :italic? true
     :superscript? true

#### Blocks
Tokens are often grouped, according to their position relative to visual boundaries (e.g. large columns of white space), into blocks.
These blocks can then be assessed (often in the context of surrounding blocks) in order to deduce features, which can in 
turn be analyzed in order to classify the block.  Blocks take the form:

    {:x0 72.0032,
     :x1 553.7642211914062,
     :y0 69.00150003433228,
     :y1 143.25,
     :page-number 1,
     :contents <sequence of Tokens>
     :features {:num-datapoints 4, :num-sentences 2, ...},
     :class :paragraph}

#### Components
While many blocks are self contained, some consist of incomplete information (e.g. a single column in a table). 
Accordingly, all blocks are transformed into a set of components, which all are of the form:

    {:type <:title | :text | :table>,
     :vals <function of :type>
     :x0 72.0032,
     :x1 553.764,
     :y0 69.05,
     :y1 143.25,
     :page-number 1}

* When type = :table, :vals will be a 3-D sequence; a sequence of table rows, where a table row is a sequence of table cells and a table cell is a sequence of Tokens.
* When type = :text or type = :title, :vals will be a 2-D sequence; a sequence of lines, where a line is a sequence of Tokens.

## Limitations
Pdf-transforms was built with the primary purpose of parsing a particular type of table structure (see resources/sample_1.pdf).  We have found it 
to be more accurate than other pdf parsing libraries (commercial or open source) on such tables but less useful when dealing with significantly different table layouts.

## License

Copyright (c) 2018 Clearwater Analytics

This project is licensed under the terms of the MIT license.
