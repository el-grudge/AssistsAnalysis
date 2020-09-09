ggplot(xADataset_M) + 
  aes(x=pass.length, y=pass.angle,size=xA*10,color=group) +
  geom_point()

ggplot(xADataset_M) + 
  aes(x=startDist, y=endDist,size=xA*10,color=group) +
  geom_point()

ggplot(overTenPercent) + 
  aes(x=pass.length, y=pass.angle,size=xA*10,color=group) +
  geom_point() +
  facet_wrap(vars(play_pattern.name)) + 
  scale_colour_viridis_d()

ggplot(overTenPercent) + 
  aes(x=startDist, y=endDist,size=xA*100,color=xA) +
  geom_point() + 
  facet_wrap(vars(play_pattern.name)) + 
  scale_colour_viridis_c()

ggplot(overTenPercent) + 
  aes(x=startDist, y=endDist,size=xA*100,color=group) +
  geom_point() + 
  facet_wrap(vars(play_pattern.name)) + 
  scale_colour_viridis_d()

ggplot(overTenPercent) + 
  aes(x=start.X, y=start.Y,size=xA*100,color=group) +
  geom_point() + 
  facet_wrap(vars(play_pattern.name)) + 
  scale_colour_viridis_d()

ggplot(overTenPercent) + 
  aes(x=start.X, y=start.Y,size=xA*100,color=xA) +
  geom_point() + 
  facet_wrap(vars(play_pattern.name)) + 
  scale_colour_viridis_c()

ggplot(overTenPercent) + 
  aes(x=start.X, y=start.Y,size=xA*100,color=xA) +
  geom_point() + 
  scale_colour_viridis_c()

ggplot(overTenPercent) + 
  aes(x=end.X, y=end.Y,size=xA*100,color=xA) +
  geom_point() + 
  facet_wrap(vars(play_pattern.name)) + 
  scale_colour_viridis_c()

ggplot(overTenPercent) + 
  aes(x=end.X, y=end.Y,size=xA*100,color=xA) +
  geom_point() + 
  scale_colour_viridis_c()

ggplot(overTenPercent) + 
  aes(x=end.X, y=end.Y,size=xA*100,color=group) +
  geom_point() + 
  scale_colour_viridis_d()

ggplot(overTenPercent) + 
  aes(x=end.X, y=end.Y,size=xA*100,color=xA) +
  geom_point() + 
  facet_wrap(vars(pass.height.name)) + 
  scale_colour_viridis_c()

ggplot(overTenPercent) + 
  aes(x=end.X, y=end.Y,size=xA*100,color=group) +
  geom_point() + 
  facet_wrap(vars(pass.height.name)) + 
  scale_colour_viridis_d()


ggplot(overTenPercent) + 
  aes(x=start.X, y=pass.angle,size=xA*100,color=group) +
  geom_point() + 
  facet_wrap(vars(pass.height.name)) + 
  scale_colour_viridis_d()

ggplot(overTenPercent) + 
  aes(x=start.X, y=pass.angle,size=xA*100,color=xA) +
  geom_point() + 
  facet_wrap(vars(pass.height.name)) + 
  scale_colour_viridis_c()
