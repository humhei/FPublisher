namespace FPublisher
module Types = 
    type EnvironmentConfig =
        { NugetApiKey: string
          GitHubToken: string
          GitHubReleaseUser: string }