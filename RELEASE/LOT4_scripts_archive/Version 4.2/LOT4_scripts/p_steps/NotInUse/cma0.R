my_cma<-CMA0(
  data = my_data,
  ID.colname = "person_id",
  event.date.colname =  "date_dispensing" ,
  event.duration.colname = "assumed_duration" ,
  event.daily.dose.colname = NA,
  medication.class.colname = "Code",
  medication.groups = NULL,
  flatten.medication.groups = FALSE,
  medication.groups.colname = NA,
  carryover.within.obs.window = T,
  carryover.into.obs.window = T,
  carry.only.for.same.medication = T,
  consider.dosage.change = NA,
  followup.window.start = 0,
  followup.window.start.unit = c("days", "weeks", "months", "years")[1],
  followup.window.start.per.medication.group = FALSE,
  followup.window.duration = 365 * 11,
  followup.window.duration.unit = c("days", "weeks", "months", "years")[1],
  observation.window.start = 0,
  observation.window.start.unit = c("days", "weeks", "months", "years")[1],
  observation.window.duration = 365 * 2,
  observation.window.duration.unit = c("days", "weeks", "months", "years")[1],
  date.format = "%m/%d/%Y",
  summary = "Base CMA object",
  suppress.warnings = FALSE,
  arguments.that.should.not.be.defined = NULL
)
pdf(plot_folder)
plot(my_cma)
dev.off
