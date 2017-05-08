import json
import random
from os import listdir


IMAGES_DIR = "images"
with open('names.txt') as f:
    names = [n.strip() for n in f.readlines()]


def get_images():
    return [f for f in listdir(IMAGES_DIR) if not f.startswith('.')]


def generate_name():
    return random.choice(names)


def create_cat(cat_id, image):
    name = generate_name()
    age = random.randint(0, 25)
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
        cat = create_cat(i, image)
        cats.append(cat)

    return json.dumps(cats)


print(generate_database())
