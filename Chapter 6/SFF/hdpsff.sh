#!/usr/bin/env bash

# Streamlined version to fit an Online Hierarchical Dirichlet Process and
# do the post-process according to the corpus

# Replace the given path with the actual path where the directories or files are

python ../Python\ dependencies/text2ldac-master/text2ldac.py -o ./lda ./data

echo "Renaming lda files according to upper directory"

cd lda
for d in *; do mv "$d" "`echo $d | sed 's/data/sff/'`"; done
cd ..

echo "Fitting Hierarchical Dirichlet Process"

python ../Python\ dependencies/online-hdp-master/run_online_hdp.py \
--W 14922 --D 130 --tau 64 --kappa 2 --batchsize 16 \
--max_time 21600 --var_converge 1e-8 --corpus_name sff \
--data_path ./lda/sff.dat --directory . --save_lag 10000000 \
--alpha 10 --gamma 1.0 --eta 0.1

echo "Getting the words for all topics"

cd sff-kappa-2.0-tau-64-batchsize-16
python ../../Python\ dependencies/onlineldavb-master/printtopics.py \
../lda/sff.vocab ./final.topics > sff-kappa-2.0-tau-64-batchsize-16.txt

echo "Splitting big file into a file by topic"

awk -v RS= '{print > ("topic-" NR ".txt")}{close("topic-" NR ".txt")}' \
sff-kappa-2.0-tau-64-batchsize-16.txt

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
