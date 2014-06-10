# Adepts developer guidelines

## Start

Hey there! We are glad you want to contribute to making Adept better! There are lots of places to start but
the perhaps most important of them is imports: from Ivy, from Maven, from NPM, or any other dependency manager
that could be more adept.

The rules are as follows:
- KISS

## Some advice

Our current code is in post-"I know all the features so I will use them all" Scala, but there is a lot that
can be done to make it better. Our goal is to write code that is readable for anybody wanting to contribute.
That does not mean we do not care about performance and how easy to refactor code. This also means alot to us,
but **readability** trumphs everything. It is up to you to decide where the line goes, and what it means for
something to be readable.

However, in Scala, readable code often (but not always) means:
- Prefer composition over traits, because it makes it easier to figure out what is going on.
- Avoid too many layers of abstraction. In particular do not create deep hierarchies of inheritence/mixins.
Deep hierarchies of traits makes it hard to figure out which method is called and where to look for the
implementation.
- Keep the amount of redirection to a minimum. For example you should (try to) avoid creating methods if
the code is not called more than once. If a method or a value has to be called from somewhere else, try to
keep all the code in one place.
- Use private[adept] on anything you do not want to expose. It makes it possible to hack around the private
but it makes it harder to do it (and nobody should complain if you break compatibility when changing it).
- Keep code that is related close. Seems obvious but it is easy to forget. It really makes it easier to
understand what is going at a glance.
- Be mindful of functional operators because they can make it hard for newbies to read. Example: does this
foldLeft really make the code easier to read and refactor or do you convolute code.
- Avoid long and complex anonymous functions - create a name for them instead.
- Do not use vars, unless it helps readibility (I want to say performance as well, but usually vars do not
help either well). Be really careful when it comes mutable state though - so avoid vars outside of methods.
- If something is done to improve performance, measure it. And make reproducible.
- Remember that tests add to LOC as well.
- Ideal code is code that breaks early: if used in the wrong way it should fail on compile or throw an
exception as early as possible if that is not possible.
- Be a defensive programmer: if you see there is a possibility that something can end up with a result
that is not what you expect, throw an exception.
- Add just enough (more is better) information to your exceptions so it easier to figure out what happened.
- Comments are good, documentation is better,  but nothing is as good as self-documenting code.
- That is it! Be a boy scout, make the code better than how you found it and be a happy hacker :)
