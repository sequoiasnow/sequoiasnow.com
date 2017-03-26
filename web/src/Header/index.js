import React from 'react';
import { Link }  from 'react-router';
import styles from './styles.scss';
import NavigationSVG from './navigation.svg';

const Header = () => {
  return (
    <section className={styles.header}>
      <NavigationSVG />
    </section>
  )
};

export default Header;
