#!/usr/bin/env bash

echo "Download and process NCBI taxonomy"
#download the ncbi taxonomy files from the FTP server and extract the file containing the names
wget https://ftp.ncbi.nih.gov/pub/taxonomy/taxdump.tar.gz
tar xfz taxdump.tar.gz names.dmp

#convert the weird ncbi taxonomy delimiters to a standard tab-delimited format
sed -E 's/\t\|\t/\t/g' names.dmp | sed -E 's/\t\|//g' > ncbi_tab_delimited.tsv

#create a list with only scientific names and sort them by those
grep "scientific name" ncbi_tab_delimited.tsv | sort -t $'\t' -k 2,2 > scientific_sorted

#create a list with only common names and sort by taxid
grep "common name" ncbi_tab_delimited.tsv| sort -t $'\t' -k 1,1 > common_sorted


echo "process gprofiler"

#make unix-like endings if needed
sed -i 's/\r//g' gprofiler.tsv

#sort by scientific name
sort -k 2,2 -t $'\t' gprofiler.tsv > gprofiler_sorted

# join with taxids and format them accordingly
echo -e "ncbi\tdisplay_name\tscientific_name\tid\tversion\tdatasources" > gprofiler_with_taxids.tsv
join -t $'\t' -1 2 -2 2 gprofiler_sorted scientific_sorted |awk -F "\t" '{printf("%s\t%s\t%s\t%s\t%s\t%s\t%s\n", $6, $1, $2, $3, $4, $5, $7)}' >> gprofiler_with_taxids.tsv

echo "process agotool"
#download species list from STRING
wget https://stringdb-static.org/download/species.v11.5.txt
#sort by taxid
sort -k 1,1 -t $'\t' species.v11.5.txt > string_taxid_sorted

#get the lines that have a common name
join -1 1 -2 1 -t $'\t' string_taxid_sorted common_sorted | sort -k 1,1 -t $'\t' -u |awk -F "\t" '{print $1"\t"$2"\t"$3"\t"$4"\t"$6"\t"$5}' > ago_common
#get the rest of them
join -1 1 -2 1 -t $'\t' -v 1 string_taxid_sorted common_sorted | sort -k 1,1 -t $'\t' -u | awk -F "\t" '{print $1"\t"$2"\t"$3"\t"$4"\t"$4"\t"$5}' > ago_rest
#concatenate and sort
cat ago_common ago_rest | sort -k 1,1 -t $'\t' -r  > ago_string_display_names

#download additional species from Viruses.STRING
wget http://viruses.string-db.org/download/species.v10.5.txt
#sort by taxid
sort -k 1,1 -t $'\t' species.v10.5.txt | awk '{print $_"\tViruses"}' > viruses_string_taxid_sorted

#get the lines that have a common name
join -1 1 -2 1 -t $'\t' viruses_string_taxid_sorted common_sorted | sort -k 1,1 -t $'\t' -u |awk -F "\t" '{print $1"\t"$2"\t"$3"\t"$4"\t"$6"\t"$5}' > ago_common
#get the rest of them
join -1 1 -2 1 -t $'\t' -v 1 viruses_string_taxid_sorted common_sorted | sort -k 1,1 -t $'\t' -u | awk -F "\t" '{print $1"\t"$2"\t"$3"\t"$4"\t"$4"\t"$5}' > ago_rest
#concatenate and sort
cat ago_common ago_rest | sort -k 1,1 -t $'\t' -r  |grep -v "#" > ago_viruses_string_display_names

#concatenate and remove duplicates
cat ago_string_display_names ago_viruses_string_display_names| sort -t $'\t' -k 1,1 -ru | cut -f 1,2,4,5,6 | awk -F "\t" '{if ($4==""){print $1"\t"$2"\t"$3"\t"$3"\t"$5}else {print $_}}' > agotool_display_names.tsv


echo "process KEGG"
#download KEGG organisms json
wget -O KEGG.json "https://www.genome.jp/kegg-bin/download_htext?htext=br08610&format=json"

#keep the scientific names of all gprofiler and agotool lines
cut -f 1,2 gprofiler_with_taxids.tsv > gprof_tax
cut -f 1,4 agotool_display_names.tsv > ago_tax

cat gprof_tax ago_tax |sort -t $'\t' -k 1,1 -u > taxids_nr

echo -e "taxid\tscientific_name\tkegg" > KEGG_codes.tsv
python ./kegg_codes.py KEGG.json taxids_nr | sort -t $'\t' -k 1,1 -u >> KEGG_codes.tsv

echo "cleanup"
rm species.v11.5.txt
rm *_sorted
rm ago_*
rm gprof_*
rm taxids_nr
rm taxdump.tar.gz names.dmp
rm ncbi_tab_delimited.tsv
