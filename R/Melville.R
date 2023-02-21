
# downnload specified imageset from JL Melville

# current choices

# "download_cifar10"            "download_fashion_mnist"     
# "download_kuzushiji_mnist"    "download_mammoth10k"        
# "download_mammoth50k"         "download_mnist"             
# "download_norb_small"         "download_qmnist"         

getJLMImageSet <- function(imageSet) 
{
   require(umap)
   require(devtools)
   u <- require(snedata)
   if (!u) {
      devtools::install <- devtools::github("jlmelville/snedata")
   }

   cmd <- paste0('download_',imageSet,'()')
   eval(parse(text=cmd))
}

