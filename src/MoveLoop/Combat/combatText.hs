module MoveLoop.Combat.CombatText (oddVarKills, oddvarTakeDmg, oddVarMiss, oddVarCrit, oddVarAttack, critHit, missedAttack, encounterEnemy, coinSuccess, coinNeutral, coinCritical, escape, pAttack1, pAttack2, eAttack, damage, escapeFail, escapeSuccess, combatOptions1, combatOptions2, killedEnemy1, killedEnemy2, killedEnemy3, killedEnemy4) where

encounterEnemy = "You have encountered "
coinSuccess = "It has not noticed you yet..."
coinNeutral = "It has already seen you!"
coinCritical = "You have been attacked by "
combatOptions1 = "Your turn:\n(a) Attack\n(h) Heal: " --Split due to number being inserted
combatOptions2 = "\n(e) Escape"
escape = "Do you wish to escape?(y/n)"
escapeFail = "Escape failed!"
escapeSuccess = "You get away safely..."
pAttack1 = "You strike the "
pAttack2 = ", you deal "
eAttack = " attacks you, it deals "
damage = " damage."
killedEnemy1 = " has slain the "
killedEnemy2 = "!\n It dropped: "
killedEnemy3 = " gold.\n You gained: "
killedEnemy4 = " exp!"
missedAttack = " missed!"
critHit = "Critical Hit!"

oddVarAttack = ["Kor va du i -82?", "Pass på, ellers knekker æ dæ!", "Æ ska knækk dæ slik du aldri har vært knokke!"]
oddVarCrit = ["Tygg på denna, halv-staf til tryne!"]
oddVarMiss = ["FANJ, IKKJE IGJEN!"]
oddvarTakeDmg = ["KOM OG KNÆKK!", "STÆL DÆ I KNÆKKEKØEN"]
oddVarKills = "Du har vorte knokke..."