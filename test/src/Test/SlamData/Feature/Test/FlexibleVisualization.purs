{-
Copyright 2017 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Test.SlamData.Feature.Test.FlexibleVisualation where

import SlamData.Prelude

import Data.StrMap as SM
import Data.String as S
import Selenium.Monad (script, tryRepeatedlyTo)
import Test.Feature.Log (successMsg)
import Test.Feature.Scenario (KnownIssues, noIssues, scenario)
import Test.SlamData.Feature.Expectations as Expect
import Test.SlamData.Feature.Interactions as Interact
import Test.SlamData.Feature.Monad (SlamFeature, getConnector)

variablesChartScenario ∷ String → KnownIssues → SlamFeature Unit → SlamFeature Unit
variablesChartScenario scenarioName knownIssues implementation = do
  connector ← getConnector
  scenario
    { epic: "Flexible Visualization"
    , before: Interact.createWorkspaceInTestFolder "Flexible Visualization"
    , after: Interact.deleteFileInTestFolder "Flexible Visualization.slam"
    , title: scenarioName
    , knownIssues
    , connector
    }
    implementation

test ∷ SlamFeature Unit
test =
  variablesChartScenario "Make embeddable patients-city charts"
  (noIssues
     { marklogic = Just "https://github.com/quasar-analytics/quasar/issues/2341"
     , couchbase = Just "https://github.com/quasar-analytics/quasar/issues/2403"
      })
  do
    _ ← tryRepeatedlyTo $ script """
      var run = function() {
        var __init = echarts.init;
        echarts.init = function (el) {
          var chart = __init.call(echarts, el);
          chart.setOption = function (options) {
            delete options.grid;
            delete options.color;
            el.innerHTML = "<pre>" + JSON.stringify(options) + "</pre>";
          };
          return chart;
        };
      };
      run();
    """
    Interact.insertVariablesCardInFirstDeck
    Interact.provideApiVariableBindingsForVariablesCard "state" "Text" "CO"
    Interact.accessNextCardInFirstDeck
    Interact.insertTroubleshootCardInLastDeck
    Expect.troubleshootCardPresented
    Interact.accessNextCardInLastDeck
    Interact.insertQueryCardInLastDeck
    Interact.provideQueryInLastQueryCard $ S.joinWith " "
      $ [ "SELECT count(*) as ct, city, gender"
        , "FROM `/test-mount/testDb/patients`"
        , "WHERE state = :state"
        , "GROUP BY city, gender"
        , "ORDER BY ct DESC"
        , "LIMIT 30"
        ]

    Interact.runQuery
    Interact.accessNextCardInLastDeck
    Interact.insertBuildBarChartCard
    Expect.categoryEnabledInLastBuildChartCard
    Interact.activateCategoryForChartBuilder
    Interact.selectInChartBuilder ["city"]
    Interact.activateMeasureForChartBuilder
    Interact.selectInChartBuilder ["ct"]
    Interact.accessNextCardInLastDeck
    Interact.insertChartCardInLastDeck
    Expect.lastEChart chart_CO
    Interact.accessPreviousCardInLastDeck
    Interact.activateStackForChartBuilder
    Interact.selectInChartBuilder ["gender"]
    Interact.accessNextCardInLastDeck
    Expect.lastEChart chart_CO_gender
    Interact.setVarMapForDeck "Flexible Visualization" $ SM.singleton "state" "\"NE\""
    Expect.lastEChart chart_NE_gender
    Interact.accessWorkspaceWithModifiedURL $ S.replace (S.Pattern "NE") (S.Replacement "CO")
    Expect.lastEChart chart_CO_gender
    successMsg "** Successfully created flexible patient chart **"

chart_CO ∷ String
chart_CO =
  """{"series":[{"stack":"default stack","data":[{"value":["ANTONITO",1],"name":["key:ANTONITO"]},{"value":["ARVADA",1],"name":["key:ARVADA"]},{"value":["DEL NORTE",1],"name":["key:DEL NORTE"]},{"value":["DENVER",1],"name":["key:DENVER"]},{"value":["HENDERSON",1],"name":["key:HENDERSON"]},{"value":["JOHNSTOWN",1],"name":["key:JOHNSTOWN"]},{"value":["KIOWA",1],"name":["key:KIOWA"]},{"value":["LITTLETON",2],"name":["key:LITTLETON"]},{"value":["LONGMONT",1],"name":["key:LONGMONT"]},{"value":["MANZANOLA",1],"name":["key:MANZANOLA"]},{"value":["PITKIN",1],"name":["key:PITKIN"]},{"value":["ROCKVALE",1],"name":["key:ROCKVALE"]},{"value":["SILVERTHORNE",1],"name":["key:SILVERTHORNE"]},{"value":["VERNON",1],"name":["key:VERNON"]},{"value":["WALSH",1],"name":["key:WALSH"]}],"type":"bar"}],"legend":{"top":"bottom","left":"left","data":[],"textStyle":{"fontFamily":"Ubuntu, sans"}},"yAxis":{"splitLine":{"lineStyle":{"width":1}},"axisLine":{"lineStyle":{"width":1}},"axisLabel":{"textStyle":{"fontFamily":"Ubuntu, sans"}},"type":"value"},"xAxis":{"axisLabel":{"textStyle":{"fontFamily":"Ubuntu, sans"},"rotate":0,"interval":0},"data":["ANTONITO","ARVADA","DEL NORTE","DENVER","HENDERSON","JOHNSTOWN","KIOWA","LITTLETON","LONGMONT","MANZANOLA","PITKIN","ROCKVALE","SILVERTHORNE","VERNON","WALSH"],"boundaryGap":true,"type":"category"},"tooltip":{"trigger":"item","textStyle":{"color":"#000000","fontSize":12,"fontFamily":"Ubuntu, sans"}}}"""

chart_CO_gender ∷ String
chart_CO_gender =
  """{"series":[{"stack":"default stack","name":"female","data":[{"value":["ANTONITO",1],"name":["female","key:ANTONITO"]},null,{"value":["DEL NORTE",1],"name":["female","key:DEL NORTE"]},null,{"value":["HENDERSON",1],"name":["female","key:HENDERSON"]},{"value":["JOHNSTOWN",1],"name":["female","key:JOHNSTOWN"]},null,{"value":["LITTLETON",1],"name":["female","key:LITTLETON"]},{"value":["LONGMONT",1],"name":["female","key:LONGMONT"]},{"value":["MANZANOLA",1],"name":["female","key:MANZANOLA"]},null,null,null,null,{"value":["WALSH",1],"name":["female","key:WALSH"]}],"type":"bar"},{"stack":"default stack","name":"male","data":[null,{"value":["ARVADA",1],"name":["male","key:ARVADA"]},null,{"value":["DENVER",1],"name":["male","key:DENVER"]},null,null,{"value":["KIOWA",1],"name":["male","key:KIOWA"]},{"value":["LITTLETON",1],"name":["male","key:LITTLETON"]},null,null,{"value":["PITKIN",1],"name":["male","key:PITKIN"]},{"value":["ROCKVALE",1],"name":["male","key:ROCKVALE"]},{"value":["SILVERTHORNE",1],"name":["male","key:SILVERTHORNE"]},{"value":["VERNON",1],"name":["male","key:VERNON"]},null],"type":"bar"}],"legend":{"top":"bottom","left":"left","data":["female","male"],"textStyle":{"fontFamily":"Ubuntu, sans"}},"yAxis":{"splitLine":{"lineStyle":{"width":1}},"axisLine":{"lineStyle":{"width":1}},"axisLabel":{"textStyle":{"fontFamily":"Ubuntu, sans"}},"type":"value"},"xAxis":{"axisLabel":{"textStyle":{"fontFamily":"Ubuntu, sans"},"rotate":0,"interval":0},"data":["ANTONITO","ARVADA","DEL NORTE","DENVER","HENDERSON","JOHNSTOWN","KIOWA","LITTLETON","LONGMONT","MANZANOLA","PITKIN","ROCKVALE","SILVERTHORNE","VERNON","WALSH"],"boundaryGap":true,"type":"category"},"tooltip":{"trigger":"item","textStyle":{"color":"#000000","fontSize":12,"fontFamily":"Ubuntu, sans"}}}"""

chart_NE_gender ∷ String
chart_NE_gender =
  """{"series":[{"stack":"default stack","name":"female","data":[{"value":["BEE",1],"name":["female","key:BEE"]},{"value":["BLAIR",1],"name":["female","key:BLAIR"]},null,null,null,null,{"value":["OMAHA",2],"name":["female","key:OMAHA"]},null,null,{"value":["PIERCE",1],"name":["female","key:PIERCE"]},null,null,{"value":["WOOD LAKE",1],"name":["female","key:WOOD LAKE"]},{"value":["WYNOT",1],"name":["female","key:WYNOT"]}],"type":"bar"},{"stack":"default stack","name":"male","data":[{"value":["BEE",1],"name":["male","key:BEE"]},null,{"value":["CAIRO",1],"name":["male","key:CAIRO"]},{"value":["CRAIG",1],"name":["male","key:CRAIG"]},{"value":["GOTHENBURG",1],"name":["male","key:GOTHENBURG"]},{"value":["LINCOLN",1],"name":["male","key:LINCOLN"]},{"value":["OMAHA",2],"name":["male","key:OMAHA"]},{"value":["ORD",1],"name":["male","key:ORD"]},{"value":["OSHKOSH",1],"name":["male","key:OSHKOSH"]},{"value":["PIERCE",1],"name":["male","key:PIERCE"]},{"value":["SAINT EDWARD",1],"name":["male","key:SAINT EDWARD"]},{"value":["WILLOW ISLAND",1],"name":["male","key:WILLOW ISLAND"]},null,null],"type":"bar"}],"legend":{"top":"bottom","left":"left","data":["female","male"],"textStyle":{"fontFamily":"Ubuntu, sans"}},"yAxis":{"splitLine":{"lineStyle":{"width":1}},"axisLine":{"lineStyle":{"width":1}},"axisLabel":{"textStyle":{"fontFamily":"Ubuntu, sans"}},"type":"value"},"xAxis":{"axisLabel":{"textStyle":{"fontFamily":"Ubuntu, sans"},"rotate":0,"interval":0},"data":["BEE","BLAIR","CAIRO","CRAIG","GOTHENBURG","LINCOLN","OMAHA","ORD","OSHKOSH","PIERCE","SAINT EDWARD","WILLOW ISLAND","WOOD LAKE","WYNOT"],"boundaryGap":true,"type":"category"},"tooltip":{"trigger":"item","textStyle":{"color":"#000000","fontSize":12,"fontFamily":"Ubuntu, sans"}}}"""
