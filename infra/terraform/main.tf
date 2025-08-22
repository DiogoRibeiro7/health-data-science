terraform {
  required_version = ">= 1.0"
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
  }
}

provider "aws" {
  region = var.region
}

# S3 bucket for automated backups with versioning and encryption
resource "aws_s3_bucket" "hds_backup" {
  bucket = var.backup_bucket

  versioning {
    enabled = true
  }

  server_side_encryption_configuration {
    rule {
      apply_server_side_encryption_by_default {
        sse_algorithm = "AES256"
      }
    }
  }
}

variable "region" {
  type    = string
  default = "us-east-1"
}

variable "backup_bucket" {
  type = string
}
