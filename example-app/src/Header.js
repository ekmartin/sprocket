import React from 'react';
import config from './config';

const Header = () => (
  <header className="hero background-color">
    <div className="hero-head">
      <div className="container">
        <nav className="nav">
          <div className="nav-left">
            <a href="/" className="nav-item is-brand">
              <img src={`${config.apiUrl}/images/logo.png`} alt="Good Cats" />
              <h1 className="title">Good Cats ™</h1>
            </a>
          </div>
        </nav>
      </div>
    </div>
  </header>
);

export default Header;
