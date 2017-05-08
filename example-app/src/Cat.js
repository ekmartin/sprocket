import React from 'react';
import config from './config';

const Cat = ({ cat, toggleFavorite }) => {
  const heart = cat.favorite ? 'fa-heart' : 'fa-heart-o';
  return (
    <div
      onClick={() => toggleFavorite(cat)}
      className="cat card"
    >
      <div className="card-image">
        <figure className="image is-square">
          <img src={`${config.apiUrl}/images/cats/${cat.image}`} alt={cat.name} />
        </figure>
      </div>
      <div className="cat-content card-content">
        <div className="cat-box">
          <p className="cat-title title is-4">
            {cat.name}
            <span className="cat-age">
              {`(${cat.age})`}
            </span>
          </p>
          <i className={`fa cat-favorite ${heart}`}></i>
        </div>
      </div>
    </div>
  );
};

export default Cat;
