import React from 'react'
import { Link } from 'react-router'

import styles from './styles.scss'
import '../../Atoms/Normalize/styles.scss'

/* ----- Organisms ----- */
import Grid from '../../Organisms/Grid'

/* ----- Molecules ----- */
import Col from '../../Molecules/Col'
import NavigationBar from '../../Molecules/NavigationBar'
import Sidebar from '../../Molecules/Sidebar'

/* ----- Atoms ----- */
import NavigationItem from '../../Atoms/NavigationItem'
import SidebarItem from '../../Atoms/SidebarItem'

/**
 * @template
 * @description
 *  The style page is where documentation goes to live, it is simple, as documentation
 *  should be, and has no interest in data, it is presentational to the extreeme.
 */
const StylePage = (props) => {
  const { navLinks = []
        , pageLinks = []
        , children } = props 
  
  return (
    <div className={styles.stylePage}> 
      <section className={styles.navContainer}>
        <NavigationBar>
          {navLinks.map(({ href, label, selected = false }, index) => {
             return (
               <NavigationItem key={index} selected={selected}>
                 <Link to={href}>{label}</Link>
               </NavigationItem>
             )
           })}
        </NavigationBar>
      </section> 
      <section className={styles.mainContainer}> 
        <Grid gutter="5"> 
          <Col width="2">
            <Sidebar>
              {pageLinks.map(({ href, label, selected }, index) => {
                 return (
                   <SidebarItem key={index} selected={selected}>
                     <Link to={href}>{label}</Link>
                   </SidebarItem>
                 )
               })}
            </Sidebar>
          </Col>
          <Col width="6">
            {children}
          </Col>
        </Grid>
      </section>
    </div>
  )
}
export default StylePage
