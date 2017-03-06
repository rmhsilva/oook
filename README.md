# OOOK!

An ORM thing for Common Lisp web applications. Because I can. And it was fun.

`OOOK` tries to sit somewhere between the simplicity of Crane and the
usefulness of Ruby's ActiveRecord. Therefore, features include
- Clean "model" (table) definitions
- Automatic handling of model associations *if you want it*
- Serialisation / deserialisation of models for serving as JSON or building from
  POST data


## Overview

Create your models with `defmodel`:

```
(defmodel post (:belongs-to user)
  (date_published :type :timestamp)
  title
  content)

(defmodel user (:has-many spells)
  name
  (level :type :integer))
```

This creates two classes which model the "post" and "user" database tables,
including the relationship between the two.

Create a new user:

```
(defvar wizzard
  (make-instance 'user :name "Rincewind" :level 1))
```

Create a new post and add it to the user:

```
(push (posts wizzard) (make-instance 'post :title "On Staying Alive")))
```

Save the new user (and post) in the database:

```
(save! wizzard)
```

Find something:

```
(find! 'user :name :like "Rincewind")
(find! 'post :id := 2)
```


# Models

## Overview

Models are CLOS classes that create a mapping between data in a relational
table-based database (i.e. SQL) and standard common lisp types.

Like another well known library, all tables are expected to have at least these
three columns:
- A unique (for this model type) ID for the row (the table's primary key)
- Time of creation
- Time of last modification

These have historically been found to be useful in typical web applications. If
you really want to disable the second two however, you can (TODO). Every model
will always have an `id` field, however.

In addition, models you define will typically have a number of other fields,
corresponding to columns in the table, and possibly a number of *associated
models*.

### Custom slots

The "custom slots" in a model are one of two things:
- slots that map to columns in the underlying table (other than the
  aforementioned three required columns)
- slots that map to associated models

Note: foreign key columns are excluded from the first group.

### Associated models

A model can be associated to other models in a number of ways.

### Types

### Example

The `posts` table from above would have the following columns:
- id (integer, primary key)
- date_created (timestamp)
- date_modified (timestamp)
- date_published (timestamp)
- title (string)
- content (string)
- user_id (integer, foreign_key to `user` table)



## Find

The `find!` function (remember, the exclamation mark implies database access)
queries the database for models of the given type, and returns all

Normally, associated models are not populated at the same time (as there may be
many), and only the foreign keys recorded. This behaviour can be changed by
setting the optional keyword argument `populate-associated` to `t`.


## Create

It's easy to create new rows in a table. Models are instantiated with
`make-instance`, and the value of any custom slots can be specified in the
initargs. This includes associated models, which can be specified in two ways,
depending on the value provided for the relevant slot.
- if an integer (or list of integers, for *many* relationships) is provided, it
  is interpreted as a foreign key. Note: no sql is executed to retrieve the full
  model or check its existence
- a model of the appropriate type can be provided

Upon saving, a row will either be updated or created in the table.


# defmodel

Creates

```
(defmodel user (:has-one (config))
   name)
```
