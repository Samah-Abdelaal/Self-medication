# Run AFTER Calculation of Social Class

chisq.test(selfmed$Have.you.taken.selfâ..medication.in.last.three.months., selfmed$Gender)
table(selfmed$Have.you.taken.selfâ..medication.in.last.three.months., selfmed$Gender)
chisq.test(selfmed$headache, selfmed$Gender)
chisq.test(selfmed$social_class, selfmed$Gender)
chisq.test(selfmed$Do.you.check.the.prescribing.information.before.selfâ..medicating., selfmed$Gender)
chisq.test(selfmed$If.your.answer.is.Yes..always.or.Yes..sometimes.then..How.much.did.you.understand.from.the.instructions.of.prescribing.information., selfmed$Gender)
chisq.test(selfmed$X9..Have.you.ever.experienced.adverse.events.with.selfâ..medication., selfmed$Gender)
chisq.test(selfmed$genitalinf, selfmed$Gender)
chisq.test(selfmed$Have.you.ever.selfâ..medicated.yourself.with.antiâ..infective., selfmed$Gender)
chisq.test(selfmed$Did.you.have.any.infection.in.last.three.months., selfmed$Gender)

chisq.test(selfmed$genitalinf, selfmed$marital)
chisq.test(selfmed$Did.you.have.any.infection.in.last.three.months., selfmed$marital)


chisq.test(selfmed$genitalinf, selfmed$social_class)
chisq.test(selfmed$Have.you.taken.selfâ..medication.in.last.three.months., selfmed$social_class)
chisq.test(selfmed$headache, selfmed$social_class)
chisq.test(selfmed$Do.you.check.the.prescribing.information.before.selfâ..medicating., selfmed$social_class)
#
chisq.test(selfmed$If.your.answer.is.Yes..always.or.Yes..sometimes.then..How.much.did.you.understand.from.the.instructions.of.prescribing.information., selfmed$social_class)
#
chisq.test(selfmed$X9..Have.you.ever.experienced.adverse.events.with.selfâ..medication., selfmed$social_class)
chisq.test(selfmed$Have.you.ever.selfâ..medicated.yourself.with.antiâ..infective., selfmed$social_class)
chisq.test(selfmed$Did.you.have.any.infection.in.last.three.months., selfmed$social_class)
chisq.test(selfmed$Did.you.have.any.disease.in.the.last.three.months., selfmed$social_class)



fisher.test(selfmed$Have.you.taken.selfâ..medication.in.last.three.months., selfmed$Gender)
fisher.test(selfmed$headache, selfmed$Gender)
fisher.test(selfmed$social_class, selfmed$Gender)
fisher.test(selfmed$Do.you.check.the.prescribing.information.before.selfâ..medicating., selfmed$Gender)
fisher.test(selfmed$If.your.answer.is.Yes..always.or.Yes..sometimes.then..How.much.did.you.understand.from.the.instructions.of.prescribing.information., selfmed$Gender)
fisher.test(selfmed$X9..Have.you.ever.experienced.adverse.events.with.selfâ..medication., selfmed$Gender)
fisher.test(selfmed$genitalinf, selfmed$Gender)
fisher.test(selfmed$Have.you.ever.selfâ..medicated.yourself.with.antiâ..infective., selfmed$Gender)
fisher.test(selfmed$Did.you.have.any.infection.in.last.three.months., selfmed$Gender)


fisher.test(selfmed$genitalinf, selfmed$social_class)
fisher.test(selfmed$Have.you.taken.selfâ..medication.in.last.three.months., selfmed$social_class)
fisher.test(selfmed$headache, selfmed$social_class)

table(selfmed$Do.you.check.the.prescribing.information.before.selfâ..medicating., selfmed$social_class)
fisher.test(selfmed$Do.you.check.the.prescribing.information.before.selfâ..medicating., selfmed$social_class)
#
table(selfmed$If.your.answer.is.Yes..always.or.Yes..sometimes.then..How.much.did.you.understand.from.the.instructions.of.prescribing.information., selfmed$social_class)
fisher.test(selfmed$If.your.answer.is.Yes..always.or.Yes..sometimes.then..How.much.did.you.understand.from.the.instructions.of.prescribing.information., selfmed$social_class)
#?

fisher.test(selfmed$X9..Have.you.ever.experienced.adverse.events.with.selfâ..medication., selfmed$social_class)
fisher.test(selfmed$Have.you.ever.selfâ..medicated.yourself.with.antiâ..infective., selfmed$social_class)
fisher.test(selfmed$Did.you.have.any.infection.in.last.three.months., selfmed$social_class)
fisher.test(selfmed$Did.you.have.any.disease.in.the.last.three.months., selfmed$social_class)


#################

#selfmedication

selfgen <- table(selfmed$Have.you.taken.selfâ..medication.in.last.three.months., selfmed$Gender)
selfgen
round((prop.table(selfgen , 2)*100), 1)
chisq.test(selfmed$Have.you.taken.selfâ..medication.in.last.three.months., selfmed$Gender)


selfmar <- table(selfmed$Have.you.taken.selfâ..medication.in.last.three.months., selfmed$marital)
selfmar
round((prop.table(selfmar , 2)*100), 1)
fisher.test(selfmed$Have.you.taken.selfâ..medication.in.last.three.months., selfmed$marital)

selfsoc <- table(selfmed$Have.you.taken.selfâ..medication.in.last.three.months., selfmed$social_class)
selfsoc
round((prop.table(selfsoc , 2)*100), 1)
fisher.test(selfmed$Have.you.taken.selfâ..medication.in.last.three.months., selfmed$social_class)

#prescibing information

presgen <- table(selfmed$Do.you.check.the.prescribing.information.before.selfâ..medicating., selfmed$Gender)
presgen
round((prop.table(presgen , 2)*100), 1)
chisq.test(selfmed$Do.you.check.the.prescribing.information.before.selfâ..medicating., selfmed$Gender)

presmar <- table(selfmed$Do.you.check.the.prescribing.information.before.selfâ..medicating., selfmed$marital)
presmar
round((prop.table(presmar , 2)*100), 1)
fisher.test(selfmed$Do.you.check.the.prescribing.information.before.selfâ..medicating., selfmed$marital)

pressoc <- table(selfmed$Do.you.check.the.prescribing.information.before.selfâ..medicating., selfmed$social_class)
pressoc
round((prop.table(pressoc , 2)*100), 1)
fisher.test(selfmed$Do.you.check.the.prescribing.information.before.selfâ..medicating., selfmed$social_class)

#understanding information

undersgen <- table(selfmed$If.your.answer.is.Yes..always.or.Yes..sometimes.then..How.much.did.you.understand.from.the.instructions.of.prescribing.information., selfmed$Gender)
undersgen
round((prop.table(undersgen , 2)*100), 1)
fisher.test(selfmed$If.your.answer.is.Yes..always.or.Yes..sometimes.then..How.much.did.you.understand.from.the.instructions.of.prescribing.information., selfmed$Gender)

undersmar <- table(selfmed$If.your.answer.is.Yes..always.or.Yes..sometimes.then..How.much.did.you.understand.from.the.instructions.of.prescribing.information., selfmed$marital)
undersmar
round((prop.table(undersmar , 2)*100), 1)
fisher.test(selfmed$If.your.answer.is.Yes..always.or.Yes..sometimes.then..How.much.did.you.understand.from.the.instructions.of.prescribing.information., selfmed$marital)

underssoc <- table(selfmed$If.your.answer.is.Yes..always.or.Yes..sometimes.then..How.much.did.you.understand.from.the.instructions.of.prescribing.information., selfmed$social_class)
underssoc
round((prop.table(underssoc , 2)*100), 1)
fisher.test(selfmed$If.your.answer.is.Yes..always.or.Yes..sometimes.then..How.much.did.you.understand.from.the.instructions.of.prescribing.information., selfmed$social_class)

#adverse events

adversegen <- table(selfmed$X9..Have.you.ever.experienced.adverse.events.with.selfâ..medication., selfmed$Gender)
adversegen
round((prop.table(adversegen , 2)*100), 1)
fisher.test(selfmed$X9..Have.you.ever.experienced.adverse.events.with.selfâ..medication., selfmed$Gender)

adversemar <- table(selfmed$X9..Have.you.ever.experienced.adverse.events.with.selfâ..medication., selfmed$marital)
adversemar
round((prop.table(adversemar , 2)*100), 1)
fisher.test(selfmed$X9..Have.you.ever.experienced.adverse.events.with.selfâ..medication., selfmed$marital)

adversesoc <- table(selfmed$X9..Have.you.ever.experienced.adverse.events.with.selfâ..medication., selfmed$social_class)
adversesoc
round((prop.table(adversesoc , 2)*100), 1)
fisher.test(selfmed$X9..Have.you.ever.experienced.adverse.events.with.selfâ..medication., selfmed$social_class)

#anti-infective

infgen <- table(selfmed$Have.you.ever.selfâ..medicated.yourself.with.antiâ..infective., selfmed$Gender)
infgen
round((prop.table(infgen , 2)*100), 1)
chisq.test(selfmed$Have.you.ever.selfâ..medicated.yourself.with.antiâ..infective., selfmed$Gender)

infmar <- table(selfmed$Have.you.ever.selfâ..medicated.yourself.with.antiâ..infective., selfmed$marital)
infmar
round((prop.table(infmar , 2)*100), 1)
fisher.test(selfmed$Have.you.ever.selfâ..medicated.yourself.with.antiâ..infective., selfmed$marital)

infsoc <- table(selfmed$Have.you.ever.selfâ..medicated.yourself.with.antiâ..infective., selfmed$social_class)
infsoc
round((prop.table(infsoc , 2)*100), 1)
fisher.test(selfmed$Have.you.ever.selfâ..medicated.yourself.with.antiâ..infective., selfmed$social_class)
