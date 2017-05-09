import json
import random
from os import listdir

CAT_COUNT = 9
IMAGES_DIR = "images/cats"
with open('names.txt') as f:
    names = [n.strip() for n in f.readlines()]


def get_images():
    images = [f for f in listdir(IMAGES_DIR) if not f.startswith('.')]
    return images[:CAT_COUNT]


def generate_name():
    return random.choice(names)


def create_cat(cat_id, image):
    name = generate_name()
    age = random.randint(0, 10)
    return {
        'id': cat_id,
        'name': name,
        'age': age,
        'image': image,
        'favorite': False
    }


def generate_database():
    images = get_images()
    cats = []
    for i, image in enumerate(images):
        cat = create_cat(i + 1, image)
        cats.append(cat)

    return json.dumps(cats)


print(generate_database())
