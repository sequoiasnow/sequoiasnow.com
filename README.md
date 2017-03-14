# My name is Sequoia Snow, you came to my website, prepare to read!

Yes, this is a personal website and resume. I've built this site with
the latter in mind, it's goal is to showcase some very solid web 
design principles as well as backend logic. That being said, to err is
human, and my doctors tell me that I'm definitily human, so, take this
code as an example, not an ideal. 

## What is this anyway?

Well its quite simple really, this is the code for a webiste. If your
reading this I assume you have some knowledge of how that works, it 
is quite simply a frontend, a backend, and of course, a database.
In this case I've used haskell to create the backend, fround in `/api`
because of its typesafe nature it is inherently nice to work with and
makes finding bugs very easy. Haskell talks to a postgresql database
using the hasql library for easy connection. It is quite a simple, 
but interesting method of creating a setup. Now the api talks to
a variety of things, it talks to a visual client I have yet to build,
but which shall be included under clients. It also talks to an emacs
plugin which allows the site to be updated from within emacs, and 
eventually a command line plugin to do the same. All this comes
from the inherent brilliance of a secure haskell api.

## The Site

The site is of course the product of all this talk. Now the site is 
created using a variety of designs and will often change. Simply put
it consists of a primary resume page, a blog, and a contact method. 

Some of the complexity comes from the hosting of exampels along with
the blog posts. Usually the blog posts are built into the examples 
and thus contain their own javascript/css/html. 

## A blog post

Containing css/javascript/html? That seems like a bit of a tall order.
Yes. Yes it is. Now how this works is through templates. A post may
specify a template file, a javascript file and a css file. These are
then uploaded and inserted when called upon. That template file might
look something like the following...

```html
<article>
    <!-- ... -->
   <section> 
    {{content}}
   </section
    <!-- ... -->
</article>
```

Or to access the content with javascript.

```html
<script>
    window.content = `{{content}}`;
</script>
```



 






