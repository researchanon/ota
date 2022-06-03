# Open Transactional Actions: Interacting with non-transactional resources in STM Haskell

Installing the modified ghc compiler (you might have to downgrade alex, happy and autoconf):

git clone https://github.com/ghc/ghc.git
cd ghc
git checkout ghc-8.6 
git submodule update --init 
git remote add jonathas https://github.com/Jonathas-Conceicao/ghc.git 
git remote update 
git checkout handlers-implementation-8.6 
cp mk/build.mk.sample mk/build.mk 
./boot
./configure
make -j9

