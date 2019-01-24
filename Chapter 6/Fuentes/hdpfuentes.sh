#!/usr/bin/env bash

# Streamlined version to fit an Online Hierarchical Dirichlet Process and
# do the post-process according to the corpus

# Replace the given path with the actual path where the directories or files are

python ../Python\ dependencies/text2ldac-master/text2ldac.py -o ./lda ./data

echo "Renaming lda files according to upper directory"

cd lda
for d in *; do mv "$d" "`echo $d | sed 's/data/fuentes/'`"; done
cd ..

echo "Fitting Hierarchical Dirichlet Process"

python ../Python\ dependencies/online-hdp-master/run_online_hdp.py \
--D 2044 --W 3120 --tau 64 --kappa 0.6 --batchsize 256 \
--max_time 21600 --var_converge 1e-8 --corpus_name fuentes \
--data_path ./lda/fuentes.dat --directory . --save_lag 10000000 \
--alpha 0.1 --gamma 0.1 --eta 0.01 --T 10 --K 5

echo "Getting the words for all topics"

cd fuentes-kappa-0.6-tau-64-batchsize-256
python ../../Python\ dependencies/onlineldavb-master/printtopics.py \
../lda/fuentes.vocab ./final.topics > fuentes-kappa-0.6-tau-64-batchsize-256.txt

echo "Splitting big file into a file by topic"

awk -v RS= '{print > ("topic-" NR ".txt")}{close("topic-" NR ".txt")}' \
fuentes-kappa-0.6-tau-64-batchsize-256.txt

echo "Removing the first line for all topic files"

for f in topic*; do echo "$(tail -n +2 $f)" > $f; done

echo "Removing topics that are identical"

for file in ./topic*; do
variable=`md5sum $file | cut -d " " -f 1`
echo $file
echo " "
find . -type f \( -iname "*.txt" -not -path $file \) \
-print0 | xargs -0 md5sum | grep $variable | awk -F ' ' '{print $2}' | xargs rm
done

echo "Creating wordtable for all topics."

for file in topic*; do
awk -F ' ' '{print $1}' $file > $file.words
done
{paste *words} > wordtable.txt

echo "file wordtable.txt created"
echo "Removing all the file clutter"

# Comment the following line if the files for the topics are needed
rm topic*

echo "Creating similarity matrix to check the association between topics by repeated words."

Rscript --vanilla ../../Rcode/simmatrix.R ./wordtable.txt ./simmat.png

echo "Similarity matrix created."
