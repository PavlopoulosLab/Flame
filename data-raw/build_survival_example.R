# Builds the survival-analysis example: TCGA liver hepatocellular carcinoma (LIHC, PanCancer
# Atlas) from the cBioPortal API.
#   inst/extdata/examples/tcga_lihc_expression.tsv  genes x tumours, log2(RSEM + 1)
#   inst/extdata/examples/tcga_lihc_clinical.tsv    per tumour: overall and progression-free survival, stage
# Genes: the members of ten MSigDB Hallmark sets (proliferation, liver metabolism, hypoxia...),
# enough to show both poor- and good-prognosis programmes while keeping the file small.
#   Rscript data-raw/build_survival_example.R
library(httr); library(jsonlite)
api <- "https://www.cbioportal.org/api"
study <- "lihc_tcga_pan_can_atlas_2018"
call <- function(path, body = NULL, query = NULL) {
  r <- if (is.null(body)) GET(paste0(api, path), query = query, accept_json(), timeout(300))
       else POST(paste0(api, path), query = query, body = toJSON(body, auto_unbox = TRUE), content_type_json(), accept_json(), timeout(300))
  stop_for_status(r)
  fromJSON(content(r, as = "text", encoding = "UTF-8"))
}

sets <- c("HALLMARK_E2F_TARGETS", "HALLMARK_G2M_CHECKPOINT", "HALLMARK_MITOTIC_SPINDLE", "HALLMARK_MYC_TARGETS_V1",
          "HALLMARK_FATTY_ACID_METABOLISM", "HALLMARK_XENOBIOTIC_METABOLISM", "HALLMARK_BILE_ACID_METABOLISM",
          "HALLMARK_COAGULATION", "HALLMARK_HYPOXIA", "HALLMARK_GLYCOLYSIS")
h <- msigdbr::msigdbr(species = "Homo sapiens", collection = "H")
h <- h[h$gs_name %in% sets, ]
entrez <- unique(as.integer(h$ncbi_gene))

samples <- call(paste0("/sample-lists/", study, "_rna_seq_v2_mrna"))$sampleIds
expr <- call(paste0("/molecular-profiles/", study, "_rna_seq_v2_mrna/molecular-data/fetch"),
             body = list(entrezGeneIds = as.list(entrez), sampleListId = paste0(study, "_rna_seq_v2_mrna")),
             query = list(projection = "SUMMARY"))
genes <- call("/genes/fetch", body = as.list(as.character(unique(expr$entrezGeneId))), query = list(geneIdType = "ENTREZ_GENE_ID"))
expr$gene <- genes$hugoGeneSymbol[match(expr$entrezGeneId, genes$entrezGeneId)]
m <- tapply(log2(pmax(expr$value, 0) + 1), list(expr$gene, expr$sampleId), mean)
m <- m[, colSums(is.na(m)) == 0, drop = FALSE]
m <- m[apply(m, 1, sd) > 0.1 & rowMeans(m) > 1, , drop = FALSE]        # expressed and variable

cd <- call(paste0("/studies/", study, "/clinical-data"), query = list(clinicalDataType = "PATIENT", projection = "SUMMARY"))
wide <- reshape(cd[cd$clinicalAttributeId %in% c("OS_MONTHS", "OS_STATUS", "PFS_MONTHS", "PFS_STATUS", "AJCC_PATHOLOGIC_TUMOR_STAGE"),
                   c("patientId", "clinicalAttributeId", "value")], idvar = "patientId", timevar = "clinicalAttributeId", direction = "wide")
names(wide) <- sub("^value\\.", "", names(wide))
wide$STAGE <- sub("^STAGE (I{1,3}V?).*$", "\\1", wide$AJCC_PATHOLOGIC_TUMOR_STAGE)   # I, II, III, IV
wide$STAGE[!wide$STAGE %in% c("I", "II", "III", "IV")] <- NA
smp <- colnames(m)
pat <- substr(smp, 1, 12)
clin <- data.frame(sample = smp, wide[match(pat, wide$patientId), c("OS_MONTHS", "OS_STATUS", "PFS_MONTHS", "PFS_STATUS", "STAGE")],
                   check.names = FALSE, stringsAsFactors = FALSE)
keep <- !is.na(clin$OS_MONTHS) & !is.na(clin$OS_STATUS)
clin <- clin[keep, ]; m <- m[, keep, drop = FALSE]
m <- m[order(rownames(m)), , drop = FALSE]

out <- "inst/extdata/examples"
write.table(data.frame(gene = rownames(m), round(m, 2), check.names = FALSE), file.path(out, "tcga_lihc_expression.tsv"),
            sep = "\t", quote = FALSE, row.names = FALSE)
write.table(clin, file.path(out, "tcga_lihc_clinical.tsv"), sep = "\t", quote = FALSE, row.names = FALSE, na = "")
cat(nrow(m), "genes x", ncol(m), "tumours;", sum(grepl("^1", clin$OS_STATUS)), "deaths\n")
