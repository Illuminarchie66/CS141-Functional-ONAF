module Paths where
import DataTypes
import Graph

-- This is the graph of Bonnie, with the paths it can go from each edge. 
-- The ShowStage is excluded as it is unable to go back there.
bonnieGraph :: Graph Location
bonnieGraph =
    Graph
    { vertices = [DiningArea, Backstage, WestHall, SupplyCloset, WestCorner, WestOffice]
    , edges = [
        Edge DiningArea Backstage, Edge DiningArea WestHall,
        Edge WestHall SupplyCloset, Edge WestHall WestCorner,
        Edge WestCorner WestOffice,
        Edge OfficeAnim OfficeAnim
        ]
    }

-- This is the graph of Chica, with the paths it can go from each edge. 
-- The ShowStage is excluded as it is unable to go back there.
chicaGraph :: Graph Location
chicaGraph =
    Graph
    { vertices = [DiningArea, Restrooms, EastHall, EastCorner, EastOffice]
    , edges = [
        Edge DiningArea Restrooms, Edge DiningArea EastHall,
        Edge EastHall EastCorner, Edge EastHall Kitchen,
        Edge EastCorner EastOffice,
        Edge OfficeAnim OfficeAnim
        ]
    }

-- This is the path of Freddy used, where it acts more as a queue, following it in order.
-- Currently unused due to lack of testing.
freddyGraph :: Graph Location
freddyGraph =
    Graph
    { vertices = [ShowStage, DiningArea, Restrooms, Kitchen, EastCorner]
    , edges = [
        Edge ShowStage DiningArea,
        Edge DiningArea Restrooms,
        Edge Restrooms Kitchen,
        Edge Kitchen EastCorner
    ]
    }