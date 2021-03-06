package org.riedelcastro.thebeast.alchemy

/**
 * @author Sebastian Riedel
 */

trait AlchemySmokingFixtures {
  val smokingSignature = """
    // Evidence
    Friends(person, person)

    // Some evidence, some query
    Smokes(person)

    // Query
    Cancer(person)
  """

  val smokingRules = """
    // Rules
    // If you smoke, you get cancer
    Smokes(x) => Cancer(x)

    // People with friends who smoke, also smoke
    // and those with friends who don't smoke, don't smoke
    Friends(x, y) => (Smokes(x) <=> Smokes(y))
  """

  val smokingTrainData = """
    Friends(Anna, Bob)
    Friends(Bob, Anna)
    Friends(Anna, Edward)
    Friends(Edward, Anna)
    Friends(Anna, Frank)
    Friends(Frank, Anna)
    Friends(Bob, Chris)
    Friends(Chris, Bob)
    Friends(Chris, Daniel)
    Friends(Daniel, Chris)
    Friends(Edward, Frank)
    Friends(Frank, Edward)
    Friends(Gary, Helen)
    Friends(Helen, Gary)
    Friends(Gary, Anna)
    Friends(Anna, Gary)

    Smokes(Anna)
    Smokes(Edward)
    Smokes(Frank)
    Smokes(Gary)

    Cancer(Anna)
    Cancer(Edward)
  """

}