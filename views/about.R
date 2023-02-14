aboutPage <- fluidRow(
  column(
    12,
    HTML("
     <h2> About <i>Flame</i> </h2>
      <hr style=height:2px;border-width:0;color:#ffa600;background-color:#ffa600;>
      <p>Flame is actively developed and maintained by the Bioinformatics and Integrative Biology Lab</p>
      <h3> Developers </h3>
      <ul>
          <li> Evangelos Karatzas, karatzas[at]fleming[dot]gr </li>
        	<li> Fotis A. Baltoumas, baltoumas[at]fleming[dot]gr </li>
      	  <li> Georgios A. Pavlopoulos, pavlopoulos[at]fleming[dot]gr </li>
      </ul>
      <h3>Code Availability</h3>
      <p>The source code for Flame can be found in <a href='https://github.com/PavlopoulosLab/Flame/' target='_blank'>GitHub</a> repository.</p>
      <h3> Related Software </h3>
      <ul>
          <li> <a href='https://biit.cs.ut.ee/gprofiler/gost' target='_blank'>gProfiler: Functional Enrichment Analysis</a>
          <li> <a href='https://string-db.org/' target='_blank'>STRING-DB: A database of protein-protein interactions, offering network visualization options</a>
          <li> <a href='https://agotool.org/' target='_blank'>aGOtool: Protein-centric enrichment analysis and literature search with abundance-bias correction</a>
          <li> <a href='http://bib.fleming.gr:3838/OnTheFly/' target='_blank'>OnTheFly<sup>2.0</sup>: Extract Biological Information from Documents</a>
      </ul>
      <h3> Cite <i>Flame</i> </h3>
      <p style='font-size:15px'>If you find Flame useful in your work please cite:</p>
      
     &bull; F. Thanati, E. Karatzas, F. Baltoumas, D. J. Stravopodis, A. G. Eliopoulos, and G. Pavlopoulos, <b>\"FLAME: a web tool for functional and literature enrichment analysis 
      of multiple gene lists,\"</b> Biology, 2021, <a href='https://www.mdpi.com/2079-7737/10/7/665' target='_blank'>doi: 10.3390/biology10070665</a>
    ")
  )
)
