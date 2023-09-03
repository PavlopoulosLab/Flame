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
      <p>The source code for Flame can be found in <a href='https://github.com/PavlopoulosLab/Flame/' target='_blank'>this GitHub repository</a>.</p>
      <h3> Related Software </h3>
    
      <ul>
          <li> <a href='https://biit.cs.ut.ee/gprofiler/gost' target='_blank'>gProfiler: Functional Enrichment Analysis</a>
          <li> <a href='https://agotool.org/' target='_blank'>aGOtool: Protein-centric enrichment analysis and literature search with abundance-bias correction</a>
          <li> <a href='https://www.webgestalt.org//' target='_blank'>WebGestalt: WEB-based GEne SeT AnaLysis Toolkit, for functional encrichment analysis</a>
          <li> <a href='https://maayanlab.cloud/Enrichr/' target='_blank'>Enrichr: a comprehensive gene set enrichment analysis web server</a>
          <li><a href='https://bib.fleming.gr:8084/app/darling' target='_blank'>Darling: A Web Application for Detecting Disease-Related Biomedical Entity Associations with Literature Mining</a>
          <li> <a href='http://bib.fleming.gr:3838/OnTheFly/' target='_blank'>OnTheFly<sup>2.0</sup>: Extract Biological Information from Documents</a>
          <li> <a href='https://string-db.org/' target='_blank'>STRING-DB: A database of protein-protein interactions, offering network visualization options</a>
      </ul>
      <h3> Cite <i>Flame</i> </h3>
      <p style='font-size:15px'>If you find Flame useful in your work please cite:</p>
      <ul>
        <li> Karatzas, E., Baltoumas, F.A., Aplakidou, E., Kontou, P.I., Stathopoulos, P., Stefanis, L., Bagos, P.G., Pavlopoulos, G.A. (2023) <b>Flame (v2.0): advanced integration and interpretation of functional enrichment results from multiple sources.</b> 
        <i>Bioinformatics</i>. Volume 39, Issue 8, August 2023, btad490; doi: <a href='https://doi.org/10.1093/bioinformatics/btad490' target='_blank'>10.1093/bioinformatics/btad490</a>; Pubmed: <a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10423032/' target='_blank'>37540207</a> </li>
        <li> Thanati, F., Karatzas, E., Baltoumas, F.A., Stravopodis, D.J., Eliopoulos, A.G., Pavlopoulos, G.A. (2021) <b>FLAME: a web tool for functional and literature enrichment analysis 
      of multiple gene lists,</b> <i>Biology</i>. 2021; 10(7):665; doi: <a href='https://www.mdpi.com/2079-7737/10/7/665' target='_blank'>10.3390/biology10070665</a>; Pubmed: <a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8301326/' target='_blank'>34356520</a></li>
      </ul>
    ")
  )
)
