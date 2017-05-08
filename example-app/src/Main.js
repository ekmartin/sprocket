import React, { Component } from "react";
import Cat from './Cat';
import config from './config';

export default class Main extends Component {
  state = {
    cats: []
  };

  componentWillMount() {
    const url = `${config.apiUrl}/cats`;
    fetch(url)
      .then(res => res.json())
      .then(cats => this.setState({ cats }));
  }

  render() {
    return (
      <section className="section">
        <div className="container">
          <div className="cats">
            {this.state.cats.map(cat =>
              <Cat key={cat.id} cat={cat} />
            )}
          </div>
        </div>
      </section>
    );
  }
}
