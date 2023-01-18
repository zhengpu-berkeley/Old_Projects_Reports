# ZZP 2023 Modified from 
# cvstudio.getDataset

def getDataset(
    annotations,
    bucket_name=None,
    transform=None,
    train_test=None,
    percentage_train=0.8,
    random_state=0,
    degrees=5,
):
    import pandas as pd
    from PIL import Image
    from torch.utils.data import Dataset
    import os
    from pathlib import Path


    class Dataset(Dataset):
        def __init__(
            self,
            annotations,
            bucket_name,
            transform,
            train_test=None,
            percentage_train=percentage_train,
            random_state=random_state,
            degrees=degrees,
        ):

            self.train_test = train_test
            self.transform = transform
            if bucket_name is None:
                bucket_name = "default_bucket"
                print("Using default bucket name:", bucket_name)
            if self.transform is None:
                print("defult transform for pretrained model resnet18")
                self.setDefaultTransform()

            self.train_test = train_test
            self.degrees = degrees

            problem_type = annotations["type"]

            if problem_type != "classification":
                raise Exception("Can only get Dataset of Classification models")

            labels_types = annotations["labels"]

            labels_filename = Path(os.getcwd()).joinpath(bucket_name + "_lables.csv")

            if os.path.exists(labels_filename):
                self.data = pd.read_csv(labels_filename)
            else:
                print("labels_filename DNE. Creating new labels file from annotations...")
                data_ = {"label": [], "y": [], "file_name": [], "key": []}
                for key, label_dict in annotations["annotations"].items():
                    filename = Path(os.getcwd()).joinpath("images", key)

                    if os.path.exists(filename):
                        label = label_dict[0]["label"]

                        data_["label"].append(label)
                        data_["y"].append(labels_types.index(label))
                        data_["key"].append(key)
                        data_["file_name"].append(filename)

                self.data = pd.DataFrame(data_)
                # train set
            if self.train_test == "train":
                print("this is the training set")
                self.data = self.data.sample(
                    frac=percentage_train, random_state=random_state
                ).reset_index()
                labels_filename = Path(os.getcwd()).joinpath(
                    bucket_name + "_train_lables.csv"
                )
                # self.data.to_csv(labels_filename)
            # test set
            if self.train_test == "test":
                print("this is the test set")
                labels_filename = Path(os.getcwd()).joinpath(
                    bucket_name + "_test_lables.csv"
                )
                temp = self.data.sample(
                    frac=percentage_train, random_state=random_state
                )
                self.data = self.data.drop(temp.index).reset_index()
                # self.data.to_csv(labels_filename)

            self.n_classes = len(self.data["y"].unique())

        def setDefaultTransform(self):
            from torchvision import transforms

            mean = [0.485, 0.456, 0.406]
            std = [0.229, 0.224, 0.225]
            if self.train_test == "train":
                # Data augmentation and normalization for training
                self.transform = transforms.Compose(
                    [
                        transforms.Resize((224, 224)),
                        transforms.RandomHorizontalFlip(),
                        transforms.RandomRotation(degrees=5),
                        transforms.ToTensor(),
                        transforms.Normalize(mean, std),
                    ]
                )
            else:
                # Data augmentation and normalization for training
                self.transform = transforms.Compose(
                    [
                        transforms.Resize((224, 224)),
                        transforms.ToTensor(),
                        transforms.Normalize(mean, std),
                    ]
                )

        def __len__(self):
            return self.data.shape[0]

        def __getitem__(self, idx):
            image = Image.open(self.data.loc[idx, "file_name"]).convert('RGB')
            y = self.data.loc[idx, "y"]
            if self.transform:
                image = self.transform(image)

            return image, y

    dataset = Dataset(
        annotations=annotations,
        bucket_name=bucket_name,
        transform=transform,
        train_test=train_test,
        percentage_train=percentage_train,
        random_state=random_state,
        degrees=degrees,
    )
    return dataset