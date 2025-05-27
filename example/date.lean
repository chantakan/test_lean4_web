-- ===========================================
-- Lean 4 Web デートシステム 最終動作確認版
-- https://live.lean-lang.org/ で動作確認済み
-- ===========================================

-- Step 1: 基本的な型定義
inductive Weather where
  | Sunny    -- 晴れ
  | Cloudy   -- 曇り  
  | Rainy    -- 雨
  | Stormy   -- 嵐
  deriving Repr, DecidableEq

inductive Location where
  | Station      -- 駅
  | Cafe         -- カフェ
  | Restaurant   -- レストラン
  | Cinema       -- 映画館
  | Park         -- 公園
  deriving Repr, DecidableEq

-- デートの状態を表現する構造体
structure DateState where
  time : Nat              -- 時刻（14-22）
  location : Location     -- 現在地
  mood_partner : Nat      -- 相手の気分（1-10）
  budget : Nat           -- 残り予算（円）
  weather : Weather      -- 天気
  deriving Repr

-- Step 2: ヘルパー関数
def clamp (value min max : Nat) : Nat :=
  if value < min then min
  else if value > max then max
  else value

-- 安全な気分変更（常に1-10の範囲内）
def mood_change (current_mood : Nat) (change : Int) : Nat :=
  if change ≥ 0 then
    clamp (current_mood + change.natAbs) 1 10
  else
    clamp (current_mood - change.natAbs) 1 10

-- Step 3: 天気による気分への影響
def weather_impact (weather : Weather) : Int :=
  match weather with
  | Weather.Sunny  => 2    -- 晴れは気分上昇
  | Weather.Cloudy => 0    -- 曇りは変化なし
  | Weather.Rainy  => -1   -- 雨は気分低下
  | Weather.Stormy => -3   -- 嵐は大幅低下

-- Step 4: 条件チェック関数（Bool型を返す）
def can_go_cafe (state : DateState) : Bool :=
  state.time ≤ 16 && state.budget ≥ 1000 && state.weather != Weather.Stormy

def can_go_restaurant (state : DateState) : Bool :=
  state.time ≥ 18 && state.budget ≥ 3000 && state.weather != Weather.Stormy

def is_stormy (weather : Weather) : Bool :=
  match weather with
  | Weather.Stormy => true
  | _ => false

-- Step 5: 基本的な状態遷移関数

-- カフェに行く
def go_to_cafe (state : DateState) : DateState :=
  if can_go_cafe state then
    { state with 
      location := Location.Cafe,
      budget := state.budget - 1000,
      mood_partner := mood_change state.mood_partner 1,
      time := state.time + 1 }
  else 
    state  -- 条件を満たさない場合は変更なし

-- レストランに行く
def go_to_restaurant (state : DateState) : DateState :=
  if can_go_restaurant state then
    { state with 
      location := Location.Restaurant,
      budget := state.budget - 3000,
      mood_partner := mood_change state.mood_partner 3,
      time := state.time + 2 }
  else 
    state

-- 映画館に行く
def go_to_cinema (state : DateState) : DateState :=
  if state.time ≥ 18 && state.budget ≥ 2000 && !is_stormy state.weather then
    { state with 
      location := Location.Cinema,
      budget := state.budget - 2000,
      mood_partner := mood_change state.mood_partner 2,
      time := state.time + 2 }  -- 2時間映画
  else 
    state

-- 公園に行く
def go_to_park (state : DateState) : DateState :=
  if !is_stormy state.weather && state.time ≤ 18 then
    { state with 
      location := Location.Park,
      mood_partner := mood_change state.mood_partner (weather_impact state.weather + 1),
      time := state.time + 1 }
  else 
    state

-- 緊急避難（駅に戻る）
def emergency_shelter (state : DateState) : DateState :=
  { state with 
    location := Location.Station,
    mood_partner := mood_change state.mood_partner (-2),
    budget := if state.budget ≥ 500 then state.budget - 500 else 0,
    time := state.time + 1 }

-- Step 6: 判定関数（Bool型を返す）

-- デート成功判定
def is_date_successful (state : DateState) : Bool :=
  state.mood_partner ≥ 7 && 
  state.budget > 0 && 
  state.time ≤ 22

-- 災害レベル判定
def is_disaster (state : DateState) : Bool :=
  state.mood_partner ≤ 3 || state.budget = 0 || state.time > 22

-- 外出可能判定
def can_go_outdoor (weather : Weather) (time : Nat) : Bool :=
  !is_stormy weather && time ≥ 14 && time ≤ 20

-- Step 7: テスト用の状態定義

-- 初期状態
def initial_date : DateState := {
  time := 14,
  location := Location.Station,
  mood_partner := 7,
  budget := 8000,
  weather := Weather.Sunny
}

-- 時間が進んだ状態
def later_date : DateState := { 
  initial_date with time := 18 
}

-- 悪天候パターン
def stormy_date : DateState := {
  time := 15,
  location := Location.Park,
  mood_partner := 6,
  budget := 5000,
  weather := Weather.Stormy
}

-- 低予算パターン
def budget_tight : DateState := {
  time := 16,
  location := Location.Station,
  mood_partner := 6,
  budget := 2000,
  weather := Weather.Cloudy
}

-- Step 8: 実行例とテスト

-- 基本動作確認
#eval initial_date
#eval go_to_cafe initial_date
#eval go_to_restaurant initial_date  -- 時間が早いので失敗
#eval go_to_restaurant later_date    -- 18時なので成功
#eval emergency_shelter stormy_date

-- 各アクションのテスト
#eval go_to_cinema later_date
#eval go_to_park initial_date

-- 成功判定テスト
#eval is_date_successful initial_date
#eval is_date_successful (go_to_cafe initial_date)
#eval is_disaster stormy_date

-- 条件チェック
#eval can_go_cafe initial_date
#eval can_go_restaurant initial_date
#eval can_go_restaurant later_date

-- Step 9: 複雑な状態遷移

-- 複数アクションの組み合わせ
def date_sequence (state : DateState) : DateState :=
  let after_cafe := go_to_cafe state
  let evening_state := { after_cafe with time := 18 }
  go_to_restaurant evening_state

#eval date_sequence initial_date

-- 最適化されたコース
def optimal_course (state : DateState) : DateState :=
  let step1 := if can_go_cafe state then go_to_cafe state else state
  let step2 := { step1 with time := 18 }  -- 夕方まで時間を進める
  let step3 := if can_go_restaurant step2 then go_to_restaurant step2 else go_to_cinema step2
  step3

#eval optimal_course initial_date

-- 安全なデートプラン
def safe_date_plan (state : DateState) : DateState :=
  if state.time ≤ 16 && state.budget ≥ 4000 then
    let after_cafe := go_to_cafe state
    let evening_state := { after_cafe with time := 18 }
    if after_cafe.budget ≥ 3000 then
      go_to_restaurant evening_state
    else
      after_cafe
  else if is_stormy state.weather then
    emergency_shelter state
  else
    state

#eval safe_date_plan initial_date
#eval safe_date_plan budget_tight
#eval safe_date_plan stormy_date

-- Step 10: 様々なシナリオテスト

-- テストシナリオ生成
def test_scenario (time budget mood : Nat) (weather : Weather) : DateState :=
  let state : DateState := {
    time := time,
    location := Location.Station,
    mood_partner := mood,
    budget := budget,
    weather := weather
  }
  optimal_course state

-- 様々なシナリオをテスト
#eval test_scenario 14 10000 8 Weather.Sunny   -- 理想的条件
#eval test_scenario 20 2000 5 Weather.Rainy    -- 厳しい条件  
#eval test_scenario 16 8000 7 Weather.Stormy   -- 緊急事態

-- 成功率チェック
#eval is_date_successful (test_scenario 14 10000 8 Weather.Sunny)
#eval is_date_successful (test_scenario 20 2000 5 Weather.Rainy)

-- 成功確率の高いプラン検証
def success_rate_test : List Bool :=
  [
    is_date_successful (optimal_course (test_scenario 14 10000 8 Weather.Sunny)),
    is_date_successful (optimal_course (test_scenario 14 8000 7 Weather.Cloudy)),
    is_date_successful (optimal_course (test_scenario 15 6000 6 Weather.Rainy)),
    is_date_successful (optimal_course (test_scenario 16 4000 5 Weather.Stormy))
  ]

#eval success_rate_test

-- 成功したケースの数
def count_success (results : List Bool) : Nat :=
  results.filter (fun x => x) |>.length

#eval count_success success_rate_test

-- Step 11: 実用的な推奨システム

-- 予算による推奨活動
def recommend_activity (budget : Nat) (time : Nat) : Location :=
  if budget ≥ 5000 && time ≥ 17 then Location.Restaurant
  else if budget ≥ 2000 && time ≥ 18 then Location.Cinema  
  else if budget ≥ 1000 && time ≤ 16 then Location.Cafe
  else if time ≤ 18 then Location.Park
  else Location.Station

#eval recommend_activity 8000 15
#eval recommend_activity 3000 19
#eval recommend_activity 1500 20

-- 天気適応プラン
def weather_adaptive_plan (state : DateState) : DateState :=
  match state.weather with
  | Weather.Sunny => if state.time ≤ 18 then go_to_park state else state
  | Weather.Cloudy => go_to_cafe state
  | Weather.Rainy => if state.time ≥ 18 then go_to_cinema state else go_to_cafe state
  | Weather.Stormy => emergency_shelter state

#eval weather_adaptive_plan initial_date
#eval weather_adaptive_plan stormy_date

-- Step 12: 基本的な証明例（Lean 4 Web対応）

-- 緊急避難後は必ず駅にいる
theorem emergency_location (state : DateState) :
    (emergency_shelter state).location = Location.Station := by
  unfold emergency_shelter
  simp

-- カフェに行くと時間が進むか同じ
theorem time_progresses_cafe (state : DateState) :
    state.time ≤ (go_to_cafe state).time := by
  unfold go_to_cafe
  by_cases h : can_go_cafe state
  · simp [h]
  · simp [h]

-- レストランに行くと予算が減るか同じ  
theorem budget_safe_restaurant (state : DateState) :
    (go_to_restaurant state).budget ≤ state.budget := by
  unfold go_to_restaurant
  by_cases h : can_go_restaurant state
  · simp [h]
  · simp [h]

-- Step 13: ハンズオン用課題テンプレート

-- 課題1: 完璧な夜デート関数を作ろう
def perfect_evening_date (state : DateState) : DateState :=
  -- 18時以降で、予算3000円以上なら高級レストラン
  -- そうでなければ映画館
  -- どちらも無理なら駅で待機
  if state.time ≥ 18 && state.budget ≥ 3000 then
    go_to_restaurant state
  else if state.time ≥ 18 && state.budget ≥ 2000 then
    go_to_cinema state
  else
    state

#eval perfect_evening_date later_date
#eval perfect_evening_date budget_tight

-- 課題2: 完全失敗条件を定義
def is_complete_failure (state : DateState) : Bool :=
  state.mood_partner ≤ 2 || 
  state.budget = 0 || 
  state.time > 23

#eval is_complete_failure stormy_date

-- 課題3: リスク回避プラン
def risk_averse_plan (state : DateState) : DateState :=
  -- 常に安全第一：嵐なら避難、そうでなければ低コストな選択
  if is_stormy state.weather then
    emergency_shelter state
  else if state.budget ≥ 1000 && state.time ≤ 16 then
    go_to_cafe state
  else if state.time ≤ 18 then
    go_to_park state
  else
    state

#eval risk_averse_plan initial_date
#eval risk_averse_plan stormy_date

-- Step 14: 高度な分析

-- デートプランの評価指標
def evaluate_date_plan (initial : DateState) (final : DateState) : Nat :=
  let mood_score := final.mood_partner * 10
  let budget_efficiency := if initial.budget > 0 then (final.budget * 100) / initial.budget else 0
  let time_efficiency := if final.time ≤ 22 then 50 else 0
  mood_score + budget_efficiency + time_efficiency

-- 各プランの評価
#eval evaluate_date_plan initial_date (optimal_course initial_date)
#eval evaluate_date_plan initial_date (safe_date_plan initial_date)
#eval evaluate_date_plan initial_date (risk_averse_plan initial_date)

-- 最終的な成功率統計
def comprehensive_test : List (DateState × Bool) :=
  let scenarios := [
    test_scenario 14 10000 8 Weather.Sunny,   -- 理想
    test_scenario 14 8000 7 Weather.Cloudy,   -- 普通
    test_scenario 15 6000 6 Weather.Rainy,    -- やや厳しい
    test_scenario 16 4000 5 Weather.Stormy,   -- 困難
    test_scenario 18 3000 4 Weather.Rainy,    -- 夜・低予算
    test_scenario 20 1000 3 Weather.Stormy    -- 最悪
  ]
  scenarios.map (fun s => (s, is_date_successful s))

#eval comprehensive_test

-- 成功率計算
def success_percentage (results : List (DateState × Bool)) : Nat :=
  let total := results.length
  let successes := (results.filter (fun (_, success) => success)).length
  if total > 0 then (successes * 100) / total else 0

#eval success_percentage comprehensive_test

-- ===========================================
-- 総合まとめ
-- ===========================================

-- 1. 型安全プログラミング（Weather, Location, DateState）
-- 2. 関数型思考（純粋関数による状態遷移）
-- 3. 条件分岐と論理演算（Bool型の活用）
-- 4. パターンマッチング（match式）
-- 5. 構造体更新（{ state with ... }）
-- 6. 簡単な証明（theorem, by tactics）
-- 7. テスト駆動設計（#eval による確認）
-- 8. システムモデリング（現実の複雑さの抽象化）
