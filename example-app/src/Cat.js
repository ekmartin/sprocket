import React from 'react';
import config from './config';

const Cat = ({ cat }) => (
  <div className="cat card">
    <div className="card-image">
      <figure className="image is-square">
        <img src={`${config.apiUrl}/images/cats/${cat.image}`} alt={cat.name} />
      </figure>
    </div>
    <div className="cat-content card-content">
      <p className="title is-4">
        {cat.name}
        <span className="cat-age">
          {`(${cat.age})`}
        </span>
      </p>
    </div>
  </div>
);

export default Cat;
