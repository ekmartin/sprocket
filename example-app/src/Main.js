import React, { Component } from "react";
import Cat from './Cat';
import Fact from './Fact';
import config from './config';

export default class Main extends Component {
  state = {
    cats: []
  };

  constructor(props) {
    super(props);
    this.toggleFavorite = this.toggleFavorite.bind(this);
  }

  componentWillMount() {
    const url = `${config.apiUrl}/cats`;
    fetch(url)
      .then(res => res.json())
      .then(cats => this.setState({ cats }));
  }

  toggleFavorite(cat) {
    const updatedCat = {
      ...cat,
      favorite: !cat.favorite
    };

    const cats = this.state.cats.map(c =>
      c.id === cat.id ? updatedCat : c
    );

    this.setState({ cats });
    const url = `${config.apiUrl}/cats/${cat.id}/favorite`;
    fetch(url, {
      method: 'POST',
      body: JSON.stringify(updatedCat)
    });
  }

  render() {
    return (
      <section className="section">
        <div className="container">
          <Fact />
          <div className="cats">
            {this.state.cats.map(cat =>
              <Cat
                key={cat.id}
                cat={cat}
                toggleFavorite={this.toggleFavorite}
              />
            )}
          </div>
        </div>
      </section>
    );
  }
}
