---
layout: post
title:  "Models are programs"
date:   2026-07-02 02:20:02 +0100
categories: blog
---

## Overview
We present [dataframe-learn](https://github.com/DataHaskell/dataframe/tree/main/dataframe-learn), a machine learning library where the output of training models is symbolic expressions. We first motivate why symbolic models are important then show how we can improve symbolic models with compiler-optimization flavoured techniques.

## The importance of symbolic models

A model, in the general sense, is a useful representation of a phenomenon. Models help us distill vast, often irrelevant, information about a thing into a small subset of variables that we can then use to predict how that thing will change over time. For example, you have a clear model of how driving works. Except in extraordinary situations, you don't think about the catalytic converter and what its doing when you're driving. Your mental model of driving treats that information as being irrelevant to _how_ to make the car go where you want it to. Thinking about every detail every time would be paralytic. A good model saves you from doing that and focuses your attention only on the parts of of the process that will help you get to your goal.

A good model also teaches you something about the world. If you step on your accelerator pedal and your car doesn't move you have to revise your model of how it works. The picture you have in your head about how those two actions are related has to be updated. Is something broken? Did you do all the steps right? What new information do you need to consolidate this new information with your previous understanding of cars? A model is constantly in conversation with the world that it models.

Models themselves have representations. We use these representations to think about the consequences of a model (inference) and to understand why the model predicts such an outcome (interpretability). A symbolic model is a model that's written in manipulable symbols such as a mathematical notation or small subset of a programming language (a domain specific language).

As a simple, but illustrative example, Python's library sklearn bundles information about a model into an object whose attributes contain arrays that describe the model.

```python
import numpy as np
from sklearn.linear_model import LinearRegression

X = np.array([[1, 1], [1, 2], [2, 2], [2, 3]])

# y = 1 * x_0 + 2 * x_1 + 3
y = np.dot(X, np.array([1, 2])) + 3

reg = LinearRegression().fit(X, y)
reg.score(X, y) ## 1.0
reg.coef_ ## array([1., 2.])
reg.intercept_ ## np.float64(3.0)
reg.predict(np.array([[3, 5]])) ## array([16.])
```

This example is taken from the [sklearn documentation website](https://scikit-learn.org/stable/modules/generated/sklearn.linear_model.LinearRegression.html) with the commented variables indicating outputs.

The relationship we hope to learns can be stated as a clear mathematical expression:

`z = 1 * x + 2 * y + 3`

That equation is a model of how some value `z` is related to two other variables `x` and `y`. The linear regression object contains all the information you need to "reconstruct" the model. But it doesn't present the model as something you can understand and manipulate yourself. Suppose you wanted instead to see how `y` is related to `z` and `x`. With some basic algebra you could rewrite the equation yourself but when encapsulated in an object it isn't obvious that the model is something to be manipulated and understood.

An arithmetic rewrite is a simple example but the broader argument is that there is a wealth of really good tools in mathematics and computer science for manipulating symbolic structures. These tools are currently underutilized because we don't think of models as symbolic programs.

But isn't this just a representation issue, you may ask? We _can_ convert the sklearn model into a symbolic expression, why get hung up on the representation in the first place?

Representation informs how you think about a problem and what tools you reach for. The limits of your language are the limits of your world. Symbolic models change how we think about modelling problems and we'll show why shortly.

If your interest is piqued, great. If this screed feels too preachy for you let's change gears and look at some code.

## Thinking in symbols

### How to think about features
We'll look at two models on the same dataset that show the sorts of insights we can get from symbolic models.

We'll be using the [auto mpg dataset](https://archive.ics.uci.edu/dataset/9/auto+mpg) which contains some information about cars' properties and performance characteristics. The dataset is typically used to predict how many miles you can get from a car.

```haskell
import qualified DataFrame as D

rawMpg <-  D.readCsv "data/auto-mpg.csv"

df = rawMpg
   |> D.filterAllJust -- remove rows with nulls 
   |> D.derive "origin" (F.recodeWithDefault "Other" [(1, "USA"), (2, "Europe"), (3, "Japan")] (D.col "origin")) -- map region to an actual name
   |> D.select ["origin", "displacement", "horsepower", "weight", "acceleration", "mpg"]

D.take 5 df
```

| origin<br>Text | displacement<br>Double | horsepower<br>Double | weight<br>Double | acceleration<br>Double | mpg<br>Double |
| ---------------|------------------------|----------------------|------------------|------------------------|-------------- |
| USA            | 350.0                  | 165.0                | 3693.0           | 11.5                   | 15.0          |
| USA            | 302.0                  | 140.0                | 3449.0           | 10.5                   | 17.0          |
| USA            | 429.0                  | 198.0                | 4341.0           | 10.0                   | 15.0          |
| USA            | 454.0                  | 220.0                | 4354.0           | 9.0                    | 14.0          |
| USA            | 440.0                  | 215.0                | 4312.0           | 8.5                    | 14.0          |

We want to understand how all these attributes are related to a car's miles per gallon. Similar to the example above we can train a simple linear regression that will give us an equation.

```haskell
import System.Random (mkStdGen)
import DataFrame.LinearModel as D

(train, test) =  D.randomSplit (mkStdGen 42) 0.8 df

global =  D.fit defaultLinearConfig (D.col "mpg") (D.exclude ["origin"] train)
globalModel = D.predict global

print globalModel
```

`-0.006 * displacement + -0.05 * horsepower + -0.005 * weight + -0.14 * acceleration + 47`

sklearn would give you a similar model. The RMSE (think about this as the average of how much we are off by) is 4.1. That's not bad for a simple model. Now you have a sense of the relationship between all these variables and mpg. But notice we excluded the region. Well that's cause the region isn't a number. Even if it were it's (as it was before preprocessing) the number is used as an ID tag not a meaningful quantity so we dropped it. Can we get it back? 

In the world where models are just arrays we have some techniques we can use to make it work. The most popular is one hot encoding. You pretend each of the regions is a separate characteristic that's either present or not. The first row would change from:

| origin_USA<br>Int | origin_Europe<br>Int | origin_Japan<br>Int | origin_Other<br>Int | displacement<br>Double | horsepower<br>Double | weight<br>Double | acceleration<br>Double | mpg<br>Double |
| ---------------|---|---|----|--------------|----------------------|------------------|------------------------|-------------- |
| 1  | 0 | 0 | 0            | 350.0                  | 165.0                | 3693.0           | 11.5                   | 15.0          |

But now our representation has moved away from our understanding of the world. This is a consequence of our model forcing us to think in terms of vectors and array. What would a symbolic model reveal to us?

What we really want is a piecewise model that tells us how to predict miles per gallon in each region.

So our symbolic model would take the exact same model from before and segment it by region.

```haskell
tree =  D.fit (segmentOn (segmented defaultLinearConfig) ["origin"]) (D.col "mpg") train

treeModel =  D.predict tree
```
```
if origin .==. "Europe"
then 1.4178753359206387e-2 * displacement
  + -0.2322097840622525 * horsepower
  + -9.23379416348258e-4 * weight
  + -0.3361653704283981 * acceleration
  + 52.5357203733634
else if origin .==. "Japan"
then 0.11061394772448896 * displacement
  + -0.32304152333741804 * horsepower
  + -2.6595309851391766e-3 * weight
  + -0.4019561333576917 * acceleration
  + 57.92915594433213
else if origin .==. "USA"
then -3.0479203013825805e-2 * displacement
  + -2.3213231702478743e-2 * horsepower
  + -3.067009831944828e-3 * weight
  + -0.4529246216952674 * acceleration
  + 47.55663675102983
else -6.433493601136653e-3 * displacement
  + -5.130996586973583e-2 * horsepower
  + -5.044457948451233e-3 * weight
  + -0.1392386438754514 * acceleration
  + 47.35784648492086
```

Just from the intercepts we can already see that Japanese cars tend to have high miles per gallon and displacement is a more salient feature in predicting their miles per gallon than for other populations. The model naturally becomes a tree-like structure that we can learn from and reason about. And that modelling insight becomes more apparent when the language of the model allows us to think about the domain more.

Now our RMSE is 3.99 meaning we do slightly better at predicting gas mileage than a linear model. Can we do better?

### How to think about optimization
Tree ensembles like gradient boosted trees (there's a pretty good explanation of the topic [here](https://explained.ai/gradient-boosting/)) are typically very performant on tabular data. So in our quest to find a better model we reach for them instead.

Gradient boosting, in short, is accepting that no one tree can perfectly predict a complicated enough relationship so you let one small tree take a swing at the problem. Say it only only gets you 50% of the way there. In our example, say it was wrong on average by 6 miles per gallon. Gradient boosting accepts this model's shortcoming and says "it's okay, we'll just have another tree learn how to predict how much your usually get wrong" and so on and so on until you have a bunch of trees that together can give a solid prediction.

The rough formula for gradient boosting is:

`mpg = tree 1 + tree 2 + tree 3 + ...`

Ensemble models can be very large and contain a lot of redundant information. Each of these trees may contain a lot of variables in common. A split condition (e.g weight >= 1000) could be in all the trees. Our tree structure is secretly a directed acyclic graph (DAG) where multiple parents can share the same child node.

If we take our tree as is and run it through an interpreter we would waste a lot of cycles recalculating the same value. We want to extract [common subexpressions](https://en.wikipedia.org/wiki/Common_subexpression_elimination) then only compute them once. This is a common technique in compiler theory. We use e-graphs to extract the ensemble as a DAG and then write it out almost like one would a program. As an example:

```haskell
_v2 = weight .<=. 2500.0
_v11 = (0.1
  * (if _v2
    then 20.0
    else 30.0))
  + (0.1
    * (if _v2
      then 5.0
      else 15.0))
return _v11
```

This rewrite computes `_v2` once and shows clearly where it's used.

The code (with some parts left out for clarity) looks like:

```haskell
toPipeline  ::  Expr  Double  ->  Pipeline (Expr  Double)
toPipeline e =
    let tbl = collectConds e -- collect all split conditions in the symbolic model
    in fromDAG tbl (dagOf (toModel tbl e)))

raw <-  D.readCsvWithOpts readOptions "data/auto-mpg.csv"
(train, test) =  D.randomSplit (mkStdGen 42) 0.8 (prepare raw)

gbm =  D.fit defaultGBConfig{gbNEstimators  =  100, gbMaxDepth  =  3} (D.col @Double  "mpg") train

boosted =  D.predict gbm
optimized = toPipeline boosted
```

The original model had 119 shared columns and took 400 ms to run inference on. Our new pipeline/dag model takes just under 100ms. We can also leverage rewrite rules to extract even more common substructures from a tree. We could even start naming those quantities/interactions feeding into our understanding of the world.

## In closing
A lot of the biggest breakthroughs in fields come when we work across knowledge domains and try out each other's tool. We hope that this effort can act as a bridge between PL/compiler researchers and ML/stats researchers. We also hope that it will be fun and intuitive to use to solve your problems and help you understand them more.
