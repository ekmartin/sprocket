import React, { Component } from "react";
import config from './config';

export default class Main extends Component {
  state = {
    fact: ''
  };

  componentWillMount() {
    const url = `${config.apiUrl}/facts/random`;
    fetch(url)
      .then(res => res.text())
      .then(fact => this.setState({ fact }));
  }

  render() {
    const fact = this.state.fact ? `“${this.state.fact}”` : '';
    return (
      <div className="fact">
        <h4 className="title is-3">
          Fact of the Day
        </h4>
        <h4 className="fact-text title is-4">
          {fact}
        </h4>
      </div>
    );
  }
}
