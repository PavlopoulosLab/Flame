generateConvertPage <- function() {
  tags$div(
    fluidRow(
      tags$h3("Gene ID Conversion"),
      tags$br(),
      column(
        4,
        selectInput("gconvert_select", "Select file to convert", choices = NULL),
        actionButton("gconvert_button", "Convert IDs", class = "btn-submit"),
        tags$br()
      ),
      column(
        8,
        selectizeInput(
          "gconvert_organism",
          label = " Select input organism:",
          choices = ORGANISMS_FROM_FILE$print_name,
          multiple = F,
          selected = "Homo sapiens (Human) [NCBI Tax. ID: 9606]",
          width = "35%",
          options = list(placeholder = 'Select an option or start typing...')
        ),
        selectInput(
          "gconvert_target",
          label = "Target namespace",
          selected = "ENTREZGENE",
          width = "35%",
          choices = c(
            "ChEMBL"="CHEMBL", "Entrez Gene Name" = "ENTREZGENE", "Entrez Gene Accession" = "ENTREZGENE_ACC",
            "Entrez Gene Transcript Name" = "ENTREZGENE_TRANS_NAME", "UniProt Accession" = "UNIPROT_GN_ACC",
            "UniProt Gene Name" = "UNIPROT_GN", "EMBL Accession" = "EMBL", "ENSEMBL Protein ID" = "ENSP",
            "ENSEMBL Gene ID" = "ENSG", "ENSEMBL Transcript ID" = "ENST", "UniProt Archive" ="UNIPARC",
            "WIKIGENE ID" = " WIKIGENE", "RefSeq mRNA" = "REFSEQ_MRNA", "RefSeq mRNA Accession" = "REFSEQ_MRNA_ACC",
            "RefSeq Protein Accession" = "REFSEQ_PEPTIDE_ACC", "RefSeq Non-coding RNA Accession" = "REFSEQ_NCRNA_ACC",
            "BEEBASE" = "BEEBASE"
          )
        )
      )
    ),
    tags$hr(),
    DT::dataTableOutput("gconvert_table")
  )
}

generateOrthologyPage <- function() {
  tags$div(
    tags$h3("Orthology Search"),
    tags$br(),
    fluidRow(
      column(
        4, 
        selectInput("gorth_select", "Select file for orthology search", choices = NULL),
        actionButton("gorth_button", "Orthology search", class = "btn-submit")),
      column(
        8, 
        selectizeInput(
          "gorth_organism",
          label = " Select input organism:",
          choices = ORGANISMS_FROM_FILE$print_name,
          multiple = F,
          selected = "Homo sapiens (Human) [NCBI Tax. ID: 9606]",
          width = "35%",
          options = list(placeholder = 'Select an option or start typing...')
        ),
        selectizeInput(
          "gorth_target",
          label = " Select target organism:",
          choices = ORGANISMS_FROM_FILE$print_name,
          multiple = F,
          selected = "Mus musculus (Mouse) [NCBI Tax. ID: 10090]",
          width = "35%",
          options = list(placeholder = 'Select an option or start typing...')
        )
      )
    ),
    tags$hr(),
    DT::dataTableOutput("gorth_table")
  )
}