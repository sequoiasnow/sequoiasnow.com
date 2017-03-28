import React from 'react';
import { connect } from 'react-redux';
import { navigateTo } from '../actions';
import styles from './styles.scss';
import NavSVG from './NavSVG';

const Page = ({ navigateTo, title = 'Title Not Found Or something longer', children }) => {
  // I've written this here as the svg code is rather long...
  const onHomeClick = () => {
    console.log('Going Home...');
    navigateTo('HOME')
  };
  return (
    <section className={styles.header}> 
      <NavSVG onHomeClick={onHomeClick} title={title} />
      <section className={styles.content}>
        {children}
      </section>
    </section>
  )
};


const ConnectedPage = connect(
  () => { return {}; },
  (dispatch) => { return { navigateTo: (...args) => dispatch(navigateTo(...args)) } }
)(Page);


export default ConnectedPage;
