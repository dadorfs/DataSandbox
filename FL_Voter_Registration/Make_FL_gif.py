# %% 
from PIL import Image

# %%
# List file names
image_files = []
for year in range(1995, 2023):
    image_files.append(f"FL_maps/{year}_map.png")

# %% 
# Open images
images = [Image.open(file) for file in image_files]

# %%
# Save GIF
images[0].save('FL_Rep_Dem_Registrations_1995_to_2022.gif', save_all=True, append_images=images[1:], loop=0,duration=1000)
