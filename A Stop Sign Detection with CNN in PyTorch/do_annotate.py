def do_annotate():

    import os
    import json

    def data_annotator(image_path):
        # Get the name of the subfolder that the image is in
        subfolder_name = os.path.basename(os.path.dirname(image_path))
        # Assign a label to the image based on the subfolder name
        label = subfolder_name
        return label

    # Create a dictionary to store the annotations
    annotations = {}

    # Create a list to store the labels
    labels = []

    # Get the names of the subfolders
    subfolder_names = os.listdir('images_folders')

    # Iterate over the subfolders
    for subfolder_name in subfolder_names:
        # Get the path to the subfolder
        subfolder_path = os.path.join('images_folders', subfolder_name)
        # Check if the subfolder is a directory
        if os.path.isdir(subfolder_path):
            # Add the subfolder name to the list of labels
            labels.append(subfolder_name)
            # Get the names of the images in the subfolder
            image_names = os.listdir(subfolder_path)
            # Iterate over the images
            for image_name in image_names:
                # Get the path to the image
                image_path = os.path.join(subfolder_path, image_name)
                # Check if the image is a file
                if os.path.isfile(image_path):
                    # Get the label for the image
                    label = data_annotator(image_path)
                    # Add the image to the annotations dictionary
                    annotations[image_name] = [{"label": label}]

    # Create a dictionary to store the annotation file data
    annotation_file = {
        "version": "1.0",
        "type": "classification",
        "labels": labels,
        "annotations": annotations
    }

    # Write the annotation file to a JSON file
    with open('images/_annotations.json', 'w') as f:
        json.dump(annotation_file, f)



    import shutil

    # Set the source and destination folders
    src_folder = 'images_folders'
    dst_folder = 'images'

    # Get the names of the subfolders
    subfolder_names = os.listdir(src_folder)

    # Iterate over the subfolders
    for subfolder_name in subfolder_names:
        # Get the path to the subfolder
        subfolder_path = os.path.join(src_folder, subfolder_name)
        # Check if the subfolder is a directory
        if os.path.isdir(subfolder_path):
            # Get the names of the images in the subfolder
            image_names = os.listdir(subfolder_path)
            # Iterate over the images
            for image_name in image_names:
                # Get the path to the image
                image_path = os.path.join(subfolder_path, image_name)
                # Check if the image is a file
                if os.path.isfile(image_path):
                    # Construct the destination path for the image
                    dst_path = os.path.join(dst_folder, image_name)
                    # Copy the image from the source folder to the destination folder
                    shutil.copy(image_path, dst_path)
