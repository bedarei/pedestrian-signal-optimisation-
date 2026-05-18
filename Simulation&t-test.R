
set.seed(123)
perG<-c(21, 12, 15, 16, 23, 22, 54, 24, 16, 33, 67, 67, 83, 48, 47, 19, 77)#peds_per_green

tg<-17 #current_green_duration
tr<-82 #current_red_duration
cycle<-99
#hist(perG, breaks = 10)
#summary(perG)

#arrival rate
ped_arrival_rate<-mean(perG/cycle)
mean_interarrival_time<-1/ped_arrival_rate

mean_perG<-mean(perG)
ped_crossing_rate<-mean_perG/tg
mean_crossing_time<-1/ped_crossing_rate
#cat("checkcrossing:", ped_crossing_rate)

rho<-ped_arrival_rate/(mean_perG/cycle)
min_green<-ceiling(
  (ped_arrival_rate*cycle)/ped_crossing_rate
)

cat(sprintf("lambda = %.4f ped/sec  mean iat = %.2fs  mean crossing = %.3fs\n",
            ped_arrival_rate, mean_interarrival_time, mean_crossing_time))
cat(sprintf("rho = %.3f at current green=%ds min stable green = %ds\n\n",
            rho, tg, min_green))

#simulation
simulate_crossing<-function(simulation_duration = 1800,
    mean_interarrival=mean_interarrival_time,mean_service=mean_crossing_time,
    green_duration=tg,red_duration=tr) 
 
 {riat<-function()rexp(1, rate=1/mean_interarrival)
  rsvc<-function()rexp(1, rate=1/mean_service)
  
  clock<-0.0
  queue_length<-0L
  server_busy<-0L
  service_start<-0.0
  signal_state<-"RED"

  area_under_queue<-0.0
  prev_time<-0.0
  total_wait<-0.0
  total_served<-0L
  total_busy_time<-0.0
  max_queue<-0L

  queue_buf<-numeric(5000L)
  head_idx<-1L
  tail_idx<-0L

  enqueue<-function(t_arr) {
    tail_idx<<-tail_idx+1L
    if (tail_idx>length(queue_buf))
      queue_buf<<-c(queue_buf, numeric(5000L))
    queue_buf[tail_idx]<<-t_arr
    queue_length<<-queue_length+1L
    if (queue_length>max_queue) max_queue<<-queue_length
    #if (queue_length>200) cat("WARNINGexplodingt=", clock)
  }

  dequeue<-function() {
    val <-queue_buf[head_idx]
    head_idx<<-head_idx+1L
    queue_length<<-queue_length-1L
    val}
  t_arr<-riat()
  t_dep<-Inf
  t_sig<-red_duration
  sig_next<-"turn_green"

  update_area<-function(t_new) {
    area_under_queue<<-area_under_queue+queue_length*(t_new-prev_time)
    prev_time<<-t_new
  }
  begin_service<-function() {
    arr_t <-dequeue()
    total_wait <<-total_wait+(clock-arr_t)
    server_busy<<-1L
    service_start<<-clock
    t_dep <<-clock+rsvc()
  }
  repeat {
    t_next<-min(t_arr, t_dep, t_sig, simulation_duration)
    update_area(t_next)
    clock<-t_next

    if (clock >= simulation_duration) {
      if (server_busy == 1L)
        total_busy_time<-total_busy_time+(clock-service_start)
      break
    }
    if (clock == t_arr) {
      t_arr<-clock+riat()
      if (signal_state == "RED" ||server_busy == 1L) {
        enqueue(clock)
      } else {
        server_busy <-1L
        service_start<-clock
        t_dep <-clock+rsvc()
        total_served<-total_served+1L
      }
    } else if (clock == t_dep) {
      total_busy_time<-total_busy_time+(clock-service_start)
      total_served<-total_served+1L
      server_busy<-0L
      t_dep      <-Inf
      if (queue_length>0L&&signal_state == "GREEN") begin_service()
    } else if (clock == t_sig) {
      if (sig_next == "turn_green") {
        signal_state<-"GREEN"
        sig_next<-"turn_red"
        t_sig   <-clock+green_duration
        if (server_busy == 0L&&queue_length>0L) begin_service()
      } else {
        signal_state<-"RED"
        sig_next<-"turn_green"
        t_sig<-clock+red_duration
      }
    }
  }
  list(
    avg_waiting_time =if(total_served>0) total_wait/total_served else NA_real_,
    avg_queue_length =area_under_queue/simulation_duration,
    max_queue = max_queue,
    #utilisation = total_busy_time/simulation_duration,
    total_served = total_served)
}

run_scenario<-function(label, green_duration, red_duration,
                         n_replications = 100, sim_duration = 1800) {
  cat(sprintf("  %-50s", label))
  reps<-replicate(n_replications,simulate_crossing(simulation_duration=sim_duration,
                                                   green_duration=green_duration,
                                                   red_duration =red_duration),
                  simplify = FALSE)
  raw_wait<-sapply(reps, `[[`, "avg_waiting_time")
  row<-data.frame(Scenario=label,
    Green_s=green_duration,Red_s  =red_duration,
    Avg_Wait=round(mean(raw_wait, na.rm = TRUE), 1),
    Avg_Queue=round(mean(sapply(reps, `[[`, "avg_queue_length"), na.rm = TRUE), 2),
    Max_Queue=round(mean(sapply(reps, `[[`, "max_queue"),na.rm = TRUE), 1),
    #Util=round(mean(sapply(reps, `[[`, "utilisation"), na.rm = TRUE), 3),
    Served =round(mean(sapply(reps, `[[`, "total_served"),na.rm = TRUE), 0)
  )
  cat(sprintf("Avg_Wait = %5.1fs  Avg_Queue = %4.2f\n",row$Avg_Wait,row$Avg_Queue))
  list(summary =row,raw_wait =raw_wait)
}

#baseline model
result_baseline<-run_scenario(
  label = "Baseline: current timing (green=17s, red=82s)",
  green_duration = tg,
  red_duration=tr
)

wait_baseline <-result_baseline$raw_wait
result_baseline<-result_baseline$summary

#boxplot(wait_baseline, main="b wait distr..")
#cat("b.. median:", median(wait_baseline), "vs mean:", mean(wait_baseline), "\n")

#scenario testing
green_candidates<-c(17, 26, 30, 40, 50, 60)

cat("\nScenario testing\n")
scenario_results<-do.call(rbind, lapply(green_candidates, function(tg) {
  run_scenario(label = paste0("fixed green=", tg, "s"),
               green_duration = tg,
               red_duration=cycle-tg)$summary}))

closest_to_30 <-scenario_results[which.min(abs(scenario_results$Avg_Wait-30)), ]
optimised_green<-closest_to_30$Green_s
optimised_red <-closest_to_30$Red_s

cat(sprintf("\nGreen duration closest to 30s target: tg=%ds (Avg_Wait=%.1fs)\n\n",
            optimised_green, closest_to_30$Avg_Wait))

#optimised model
result_optimised<-run_scenario(
  label = paste0("Optimised: fixed timing (green=", optimised_green, "s)"),
  green_duration = optimised_green,red_duration=optimised_red )
wait_optimised <-result_optimised$raw_wait
result_optimised<-result_optimised$summary

cat("\nScenario testing results:\n")
print(scenario_results[, c("Green_s","Red_s","Avg_Wait","Avg_Queue","Max_Queue")])

cat("\nFinal comparison:\n")
both<-rbind(result_baseline, result_optimised)
print(both[, c("Scenario","Green_s","Avg_Wait","Avg_Queue","Max_Queue")])

# utilization factor (rho) comparison
rho_baseline<-ped_arrival_rate/(ped_crossing_rate*tg/cycle)
rho_optimised<-ped_arrival_rate/(ped_crossing_rate*optimised_green/cycle)

cat(sprintf("\nUtilization factor (rho):\n"))
cat(sprintf("  Baseline rho = %.3f\n", rho_baseline))
cat(sprintf("  Optimised rho = %.3f\n", rho_optimised))

cat(sprintf("\nImprovement: Wait %s%%  Queue %s%%\n",
            round((result_baseline$Avg_Wait -result_optimised$Avg_Wait)/result_baseline$Avg_Wait *100, 1),
            round((result_baseline$Avg_Queue-result_optimised$Avg_Queue)/result_baseline$Avg_Queue*100, 1)))


#t-test
t_result<-t.test(wait_baseline, wait_optimised)
print(t_result)
cat(sprintf("p-value = %.6f\n", t_result$p.value))
cat(sprintf("Conclusion: %s\n",
            ifelse(t_result$p.value < 0.05,"Significant improvement (p < 0.05)",
                   "No significant difference (p >= 0.05)")))
#par(mfrow = c(2, 2))
#Plots
plot(scenario_results$Green_s, scenario_results$Avg_Wait,
     type = "b", pch = 19, col = "steelblue", lwd = 2,
     xlab = "Green Duration (s)", ylab = "Average Waiting Time (s)",
     main = "Avg Waiting Time vs Green Duration")

segments(x0 = tg, y0 = par("usr")[3],x1 = tg, y1 = par("usr")[4],
         col = "red", lty = 2, lwd = 1.5)
segments(x0 = optimised_green, y0 = par("usr")[3],x1 = optimised_green,
         y1 = par("usr")[4],col = "darkgreen", lty = 2, lwd = 1.5)
segments(x0 = par("usr")[1], y0 = 30,x1 = par("usr")[2], y1 = 30,
         col = "orange", lty = 3, lwd = 1.5)
legend("topright", bty = "n",
       legend = c("Avg waiting time",paste0("Current tg=", tg, "s"),
                  paste0("Optimised tg=", optimised_green, "s"),"30s target"),
       col=c("steelblue", "red", "darkgreen", "orange"),
       lty=c(1, 2, 2, 3), lwd = 2)

plot(scenario_results$Green_s, scenario_results$Avg_Queue,
     type = "b", pch = 19, col = "darkorange", lwd = 2,
     xlab = "Green Duration (s)", ylab = "Average Queue Length (ped)",
     main = "Avg Queue Length vs Green Duration")
segments(x0 = tg, y0 = par("usr")[3],x1 = tg, y1 = par("usr")[4],
         col = "red", lty = 2, lwd = 1.5)
segments(x0 = optimised_green, y0 = par("usr")[3],x1 = optimised_green,
         y1 = par("usr")[4],col = "darkgreen", lty = 2, lwd = 1.5)

legend("topright", bty = "n",
       legend = c("Avg queue",paste0("Current tg=", tg, "s"),
                  paste0("Optimised tg=", optimised_green, "s")),
       col = c("darkorange", "red", "darkgreen"), lty = c(1, 2, 2), lwd = 2)

bar_data<-rbind(
  c(result_baseline$Avg_Wait,  result_baseline$Avg_Queue),
  c(result_optimised$Avg_Wait, result_optimised$Avg_Queue)
)
barplot(t(bar_data),
        beside =TRUE,col=c("tomato", "steelblue"),
        names.arg = c(paste0("Baseline\ngreen duration =", tg, "s"),
                      paste0("Optimised\ngreen duration =", optimised_green, "s")),
        ylab = "Value", ylim = c(0, 70),main = "Baseline vs Optimised")
legend("topright", bty = "n",
       legend = c("Avg Waiting time (s)", "Avg queue length (ped)"),
       fill = c("tomato", "steelblue"))
