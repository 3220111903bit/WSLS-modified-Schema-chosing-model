# 简易模拟器 V3：基于真实模型逻辑，模拟相同 payoff 下 5 个 schema 中一个高 confidence，其余低 confidence 时被选中的概率

set.seed(42)
library(dplyr)

# 模型参数设置
n_sim <- 50           # 模拟次数
n_schema <- 5           # schema 个数（同一 payoff）
items_per_schema <- 4   # 每个 schema 对应的 item 数量
schema_conf <- c(0.7, rep(0.4, n_schema - 1))   # 第1个 schema conf=1，其余为0.3
schema_var <- rep(0.1, n_schema)
item_thres <- 2.5         # item 识别阈值
schema_thres <- 5         # schema evidence 阈值
model_timestep <- 20
max_time <- 5000
Phi <- 0.8

# softmax attention（基于 item evidence）
softmax <- function(x) {
  x <- x - max(x)
  exp_x <- exp(Phi * x)
  exp_x / sum(exp_x)
}

# 模拟单次 schema selection（复刻模型结构）
simulate_once <- function() {
  # 初始化 item 信息（itemID, schemaID, evidence）
  items <- data.frame(
    ID = 1:(n_schema * items_per_schema),
    Schema = rep(1:n_schema, each = items_per_schema),
    evidence = 0,
    decision = 0
  )
  
  schema_evidence <- rep(0, n_schema)
  schema_chosen <- NA
  time <- 0
  
  while (time < max_time && is.na(schema_chosen)) {
    # schema evidence 汇总自其所属的 item
    schema_evidence <- items %>% group_by(Schema) %>% summarise(e = sum(evidence)) %>% pull(e)
    
    if (any(schema_evidence >= schema_thres)) {
      schema_chosen <- which.max(schema_evidence)
      break
    }
    
    # attention based on undecided items
    undecided <- which(items$decision == 0)
    attention_probs <- softmax(items$evidence[undecided])
    attended <- sample(undecided, 1, prob = attention_probs)
    
    sid <- items$Schema[attended]
    drift <- rnorm(1, mean = schema_conf[sid], sd = schema_var[sid])
    items$evidence[attended] <- items$evidence[attended] + drift
    
    if (items$evidence[attended] >= item_thres) {
      items$decision[attended] <- 1
    }
    
    time <- time + model_timestep
  }
  
  return(schema_chosen)
}

# 多次运行模拟
results <- replicate(n_sim, simulate_once())
prop.table(table(results))
