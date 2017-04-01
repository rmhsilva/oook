# OOOK!

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

`OOOK` is some Rails-esque ORM /magic/ on top of the venerable `CL-SQL` package,
which has been providing a solid SQL abstraction in Common Lisp for years.

The goal of `OOOK` is to greatly decrease "standard" database-driven web
application development time with the trade-off of slightly less flexiblity.
With that in mind, some of the features include:
- Clean data "model" (table) definitions
- Automatic handling of model associations (joins) during save / delete
- Serialisation / deserialisation of data for serving as JSON or building from
  POST data
- Straightforward migrations (in the future)


## Overview

Create models with `defmodel`:

~~~common-lisp
(oook:defmodel post (:belongs-to user)
  (date_published :type :timestamp)
  title
  content)

(oook:defmodel user (:has-many posts)
  "A user that writes posts"
  name
  (level :type :integer :documentation "Skill level"))
~~~

This creates two CLOS classes which model the "post" and "user" database tables,
including the relationship between the two. The models have brief dostrings,
custom slots (including types) and associations with other models.

*Note:* this really creates two `CLSQL` /view-classes/, using
`clsql:def-view-class`, containing the specified slots and a number of
additional slots for managing the joins.

### Example Workflow:

Create a new user:

~~~common-lisp
(defvar wizzard
  (make-instance 'user :name "Rincewind" :level 0))
~~~

Create a new post and add it to the user:

~~~common-lisp
(push (posts wizzard) (make-instance 'post :title "On Staying Alive")))
~~~

Save the new user (and post) in the database:

~~~common-lisp
(oook:save wizzard)  ; Will save both the user and his post
~~~

Find something, either by ID with the built-in `find-by-id` helper, or construct
a `CLSQL` statement for more complex queries (`OOOK` provides a few other
helpers, see the documentation).

~~~common-lisp
(oook:find-by-id 'post 2)  ; Find the post row with ID == 2
(clsql:select 'user)  ; Select all users
~~~

## Models Implementation Notes

### Overview

Like another well known library, all tables are expected to have at least these
three columns:
- `id`: A unique (for this model type) ID for the row (the table's primary key)
- `created-at`: Time of creation
- `last-modified`: Time of last modification

These have historically been found to be useful in typical web applications. The
`id` field is always required, and will be an index into the table. The second
two can be disabled by passing `:timestamped nil` to `defmodel`.

In addition, models you define will typically have a number of other fields,
corresponding to columns in the table, and possibly a number of /associated
models/.

### Associated models

A model can be associated to other models in a number of ways.
- belongs-to
- has-one
- owns-one
- owns-many

Models are selected exactly as you would with `CL-SQL`, and, by default, the
joins are lazily loaded (i.e. a "join" slot is only populated from the DB when
it is accessed).

The fun part is saving.

`oook:save` will save any associated models that /belong/ to the instance it is
called with. New rows will be created as necessary.

#### has-one (FK in Left)

Left requires one instance of Right, but Right doesn't care or know
about this relationship. (e.g. ingredient -> unit).

- Get: get the relevant Right and merge it into Left
- Save: only save the reference to Right
- Delete: do nothing with Right

#### owns-one (FK in right) - use with belongs-to (blargh redundancy)

Left owns one Right, and Right knows it. Left cannot have more than one
Right. (e.g. user <- config)

- Get: get the Right and merge into Left
- Save: update Right with any changes, ensure reference (ID) has not changed
- Delete: delete right as well

#### belongs-to (FK in Left) - use with owns-many or owns-one

Left is owned by one Right. Left cannot have more than one owner. (e.g. config
<- user). However Right might have more than one Left (e.g. step* <- recipe)

- Get: do not try to resolve the parent relation (circular dep)
- Save: ensure the reference to Right has not changed
- Delete: do nothing

#### owns-many (FK in Right) - use with belongs-to

Left has many Rights, and the Rights know which Left they belong to. (e.g.
recipe <- step*)

- Get: Find all Right that match Left's ID
- Save: Save new Rights in Left, delete old Rights no longer in Left
- Delete: delete all rights as well

#### many-to-many (intersection table)

*Not implemented yet*

Left references many instances of Right, and Right might be referenced by many
instances of Left. (e.g. programmer <-> project, a programmer is part of many
projects and a project has many programmers)

Get: do not try to resolve the parent relation (circular dep)