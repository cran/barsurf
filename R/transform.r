.transform = function (contrast, z1)
{	if (contrast == 0)
		z1
	else
	{	if (contrast < -1 || contrast > 0)
			stop ("constrast parameter not in interval [-1, 0]")
		contrast = 1 + contrast

		zlim = range (z1, na.rm=TRUE)
		dz = diff (zlim)
		if (dz == 0)
			z1
		else
		{	z1 = (z1 - zlim [1]) / dz
			x = as.vector (z1)
			z0 = ecdf (x)(x)
			dim (z0) = dim (z1)

			(1 - contrast) * z0 + contrast * z1
		}
	}
}

.transform.2 = function (contrast, z1, z2)
{	if (contrast != 0)
	{	if (contrast < -1 || contrast > 0)
			stop ("constrast parameter not in interval [-1, 0]")
		contrast = 1 + contrast

		x = c (as.vector (z1), as.vector (z2) )
		zlim = range (x, na.rm=TRUE)
		dz = diff (zlim)
		if (dz != 0)
		{	x = (x - zlim [1]) / dz
			f = ecdf (x)
			z0.1 = f (z1)
			z0.2 = f (z2)
			z1 = (z1 - zlim [1]) / diff (zlim)
			z2 = (z2 - zlim [1]) / diff (zlim)

			dim (z0.1) = dim (z1)
			dim (z0.2) = dim (z2)

			z1 = (1 - contrast) * z0.1 + contrast * z1
			z2 = (1 - contrast) * z0.2 + contrast * z2
		}
	}
	list (z1=z1, z2=z2)
}
