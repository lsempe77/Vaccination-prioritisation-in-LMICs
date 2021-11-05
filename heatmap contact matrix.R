ind<-squire::get_mixing_matrix(country = "India")

ggplot(ind) + geom_tile(aes())

heatmap(ind)

ggplot(reshape2::melt(ind), aes(Var1,Var2, fill=value)) + geom_tile() +
  scale_fill_viridis_c() + theme_light()

